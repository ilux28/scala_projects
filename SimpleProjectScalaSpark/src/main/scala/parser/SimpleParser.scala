package parser

import scala.collection.mutable
import scala.util.parsing.combinator._

class SimpleParser extends RegexParsers {

  var bufferIndexLog = mutable.Buffer[String]()
  var bufferOtherSentence = mutable.Buffer[String]()

  //terminals  [\s]$
  //Block defs for index expressions
  //Group phrases for indexExpression
  def `anySentenceWithoutGap` = {
    """[^"'\s]+""".r ^^ (
      _.toString
      )
  }

  def `anySentenceWithColWithoutGap` = {
    """[^\s]+""".r ^^ (
      _.toString
      )
  }

  def `anySentenceWithoutGapElse` = """\S+""".r ^^ (_.toString)

  def `anyWithCompare` = "\\S+\\s?[=<>]".r ^^ (_.toString)

  def `index`: Parser[String] = "index\\s?=".r //^^ (_ => "index=") //cases expression with index

  def `index=log` = `index` ~> `anySentenceWithoutGapElse` ^^ (_.toString)

  def `indexUniversal`: Parser[String] = "index\\s?=\\s?\\S+".r ^^ { str => {
    parse(`index=log`, str) match {
      case Success(result, _) => {
        bufferIndexLog += result
        result
      }
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }}

  /**
   *  New Functional
   * @return
   */
  //Boolean expressions
  def `anyBooleanSentence` = "AND|OR|NOT".r ^^ { str => {
    val strRes = if (str == "NOT") s"!(" else str
    strRes
  } }

  def `anyRequests` = "\\*?G\\*?E\\*?T\\*?+|\\*?P\\*?O\\*?S\\*?T\\*?+".r ^^ { str => {
    val strRequest = if (str.contains("*")) {
      val tempStr = str.replaceAll("""\*""", """.*""")
      s"_raw rlike \\'$tempStr\\'"
    } else {
      s"_raw like \\'%$str%\\'"
    }
    strRequest
  } }

  def `commonColWithCompare` = """\S+\s?[=<>]""".r ^^ (_.toString)


  def `lazyQuantifierWithBrackets` =
    """["|'].+?["|']""".r  ^^ {
      _.toString//replaceAll(""""|'""", """\""").trim
    }

  def `lazyQuantifierInnerShieldedBrackets` =
    """["|'][^\\"|'].+?[^\\"|']["|']""".r ^^ { str => {
      str//.substring(1, str.length - 1)
    } }

  def `sentenceWithRightBrackets` =
    """[^"|']+["|']""".r  ^^ {
      _.toString
    }

  def `sentenceWithLeftBrackets` =
    """["|'][^"|'\s]+""".r  ^^ {
      _.toString
    }

  /***
   * Any various colSentence
   * @return
   */

  def `sentenceWithCol` = `commonColWithCompare`.? ~ `anyVariantsWithBrackets` ^^ {
    case colOpt ~ anySent => {
      val col = colOpt.fold("")(_.toString)
      val resStr = if (col.contains("index")) {
        parse(`index=log`, s"$col$anySent") match {
          case Success(result, _) => {
            bufferIndexLog += result
            result
          }
          case failure: NoSuccess => scala.sys.error(failure.msg)
        }
        ""
      } else {
        if (anySent.contains("""\*""")) {
          s"$col\'${anySent.replaceAll("""\\\*""", """*""").replaceAll(""""""", """""")}\'"
        } else if (anySent.contains("""*""")) {
          s"${col.substring(0, col.length - 1)} rlike \\'${anySent.replaceAll("""\*""", """.*""").replaceAll(""""""", """""")}\\'"
        } else {
          if (col == "") {
            s"_raw like \\'%${anySent.substring(1, anySent.length - 1)}%\\'"
          }
          else {
            "%s%s".format(col, anySent.replaceAll("(\"|\')", """\\'"""))
          }
        }
      }
      resStr
    }
  }

  def `anyVariantsWithBrackets` =
    `lazyQuantifierInnerShieldedBrackets` |
      `lazyQuantifierWithBrackets` |
      `sentenceWithRightBrackets` |
      `sentenceWithLeftBrackets` |
      `anySentenceWithColWithoutGap`

  def `commonSentenceWithBrackets` =
    `anyBooleanSentence` |
      `anyRequests` |
      `sentenceWithCol`|
      `anySentenceWithoutGap`

  def `basicBodySentence` = rep1(`commonSentenceWithBrackets`) ^^ {
    innerListSentence => {
      val listSentenceStr = innerListSentence.map(str => {
        str
      })
      bufferOtherSentence.insertAll(0, listSentenceStr)
      listSentenceStr
    }
  }

  def `universalSentence` =
  `indexUniversal` |
    `basicBodySentence`

  /***
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
        } else if (bufferOtherSentence(x).contains("_raw") && x != 0) {
          if (bufferOtherSentence(x - 1) != "AND" && bufferOtherSentence(x - 1) != "OR") bufferOtherSentence(x) = s"AND ${bufferOtherSentence(x)}"
        }
      }
      if (i != -1) bufferOtherSentence.remove(i)
      println(bufferIndexLog.mkString(" "))
      println(bufferOtherSentence.mkString(" "))
 //     println(bufferOtherSentence.last) //.split(" ").mkString(" "))
      val resBufferOtherSentence =  bufferOtherSentence.map(_.trim).mkString(" ").trim//.replaceAll("\"","\\\\'")
      IndexApacheLog(bufferIndexLog, resBufferOtherSentence) //.toString//.mkString(" ").toString
  }
  /*  ____________________________________________________________  */

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
    val str = "GET AND POST index= test"
    val simpleParser = new SimpleParser
    println(simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str))
  }
}