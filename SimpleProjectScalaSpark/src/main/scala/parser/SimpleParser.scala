package parser

import scala.collection.mutable
import scala.util.parsing.combinator._

class SimpleParser extends RegexParsers {

  var bufferPreRez = mutable.Buffer[String]()
  var bufferIndexLog = mutable.Buffer[String]()
  var bufferOtherSentence = mutable.Buffer[String]()

  //terminals  [\s]$
  //Block defs for index expressions
  //Group phrases for indexExpression
  def `anySentenceWithoutGap` = """\S+""".r ^^ (_.toString)

  def `anyWithCompare` = "\\S+\\s?[=<>]".r ^^ (_.toString)

  def `index`: Parser[String] = "index\\s?=".r //^^ (_ => "index=") //cases expression with index

  def `index=log` = `index` ~> `anySentenceWithoutGap` ^^ (_.toString)

  def `indexUniversal`: Parser[String] = "index\\s?=\\s?\\S+".r ^^ { str => {
    parse(`index=log`, str) match {
      case Success(result, _) => {
        bufferIndexLog += result
        result
      }
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }}

  def `anySentenceWithBrackets` = "[\'\"]".r.? ~ rep1(`anySentenceWithoutGap`) ~ "[\'\"]".r.? ^^ {
    case br1 ~ listSent ~ br2 =>  {
      val strRes = if (br1.fold("")(str => str.toString) != "" && br2.fold("")(str => str.toString) != "") {
        listSent.mkString(" ").replaceAll(""""'""", """\'""")
      } else listSent.mkString(" ")//.replaceAll(""""'""", """\'""")
      bufferOtherSentence += strRes
      strRes
    }
  }

  /**
   *  New Functional
   * @return
   */
  //Boolean expressions
  def `anyBooleanSentence` = "AND|OR|NOT".r ^^ { str => {
    val strRes = if (str == "NOT") s"!(" else str
    bufferOtherSentence += strRes
    strRes
  } }

  def `anyRequests` = "\\*?G\\*?E\\*?T\\*?+|\\*?P\\*?O\\*?S\\*?T\\*?+".r ^^ { str => {
    val strRequest = if (str.contains("\\*")) {
      val tempStr = str.replaceAll("""\*""", """.*""")
      s"_raw rlike \'$tempStr\'"
    } else s"_raw like \'%$str%\'"
    bufferOtherSentence += strRequest
    strRequest
  } }

  def `commonColWithCompare` = """\S+\s?[=<>]""".r ^^ (_.toString)

  def `sentenceInnerBrackets` = `sentenceInnerBracketsBasic` ^^ { str => {
   val tempStr = s"_raw like \'%$str%\'"//{str.substring(1, str.size - 2)}%\'"
    bufferOtherSentence += tempStr
    tempStr
  }  }

  def `sentenceInnerBracketsBasic` = """"|'""".r ~ rep1(`anySentenceWithoutGap`) ~ """"|'""".r  ^^ {
    case br1 ~ listSent ~ br2 => listSent.mkString(" ")
  }

  def `colWithCompareWithBrackets` = `commonColWithCompare` ~ `sentenceInnerBracketsBasic` ^^ {
    case col ~ innerBr => {
      val resStr = if (innerBr.contains("""\*""")) {
        s"$col'${innerBr.replaceAll("""\*""", """.*""")}'"
      } else if (innerBr.contains("""*""")) {
        s"${col.substring(0, col.size - 1)} rlike \'${innerBr.replaceAll("""\\\*""", """*""")}\'"
      } else s"$col\'$innerBr\'"
      bufferOtherSentence += resStr
      resStr
    }
  }

  def `colWithCompareWithoutBrackets` = `commonColWithCompare` ~ `anySentenceWithoutGap` ^^ {
    case col ~ withoutBr => {
      val resStr = s"$col${withoutBr.replaceAll(""""|'""", """\\'""")}"
      bufferOtherSentence += resStr
      resStr
    }
  }

  def `simpleSentenceWithoutGap` = `anySentenceWithoutGap` ^^ (str => {
    bufferOtherSentence += s"$str"
    str
  })

  def `universalSentence` =
    `indexUniversal` |
      `anyBooleanSentence` |
      `anyRequests` |
      `sentenceInnerBrackets` |
      `colWithCompareWithBrackets` |
      `colWithCompareWithoutBrackets` |
      `simpleSentenceWithoutGap`

  /***
   *
   * @return
   */

  def commonSentenceTwoVariant = rep1(`universalSentence`) ^^ {
    listSentence => //listSentence//.foreach { println(_) }
      var i = -1
      for (x <- bufferOtherSentence.indices) {
        if (bufferOtherSentence(x) == "!(") {
          if (bufferOtherSentence(x + 1).contains('(')) {
            bufferOtherSentence(x + 1) == s"!${bufferOtherSentence(x + 1)}"
          } else {
            bufferOtherSentence(x + 1) = s"!(${bufferOtherSentence(x + 1)})"
          }
          i = x
        } else if (bufferOtherSentence(x).contains("_raw")) {
          if (bufferOtherSentence(x - 1) != "AND" && bufferOtherSentence(x - 1) != "OR") bufferOtherSentence(x) = s"AND ${bufferOtherSentence(x)}"
        }
      }
      if (i != -1) bufferOtherSentence.remove(i)
      println(bufferIndexLog.mkString(" "))
      println(bufferOtherSentence.mkString(" "))
      println(bufferOtherSentence.mkString("").split("").size) //.split(" ").mkString(" "))
      val resBufferOtherSentence =  bufferOtherSentence.mkString(" ")//.replaceAll("\"","\\\\'")
      IndexApacheLog(bufferIndexLog, resBufferOtherSentence) //.toString//.mkString(" ").toString
  }
  /*  ____________________________________________________________  */
  //Parsers for every phrases

  case class IndexApacheLog(bufferIndexLogs: mutable.Buffer[String], groupCol: String) {
    val apBuffer = bufferIndexLogs.map(_.stripPrefix("\"").stripSuffix("\""))
    override def toString: String = apBuffer.map(ap => s"```{'$ap' : { 'query': '$groupCol', tws: 0, twf: 0}}```").mkString(",")
  }


  /**
  * This method a compared res and str, if it is equal => return Success
  * @param str
  * @return
  */

  def parseResult(resIndex: Parser[IndexApacheLog], str: String): String = {
    parse(resIndex, str) match {
      case Success(matched, _) => matched.toString
      case Failure(msg, _) => s"FAILURE  $msg"
      case Error(msg, _) => s"Error $msg"
    }
  }
}

object SimpleParser {
  def main(args: Array[String]): Unit = {
    val str = """index=test col1= "test value""""
    val simpleParser = new SimpleParser
    println(simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str))
  }
}