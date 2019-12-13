package parser

import scala.collection.mutable
import scala.util.parsing.combinator._

class SimpleParser extends RegexParsers {

  var bufferPreRez = mutable.Buffer[String]()
  var bufferIndexLog = mutable.Buffer[String]()
  var bufferOtherSentence = mutable.Buffer[String]()
  var resBuffer = mutable.Buffer[String]()
  var bufferSimpleStringsBool = mutable.Buffer[String]()
  var bufferStringsBool = mutable.Buffer[String]()

  //terminals  [\s]$
  //Block defs for index expressions
  //Group phrases for indexExpression
  def `commonNumberChars` = "\\S+".r ^^ (_.toString)

  def `index=log` = `index` ~> `commonNumberChars` ^^ (_.toString)

  //def `colWith?Brackets` = `commonNumberChars` ~ `commonNumberChars` ^^ (_.toString)

  def `anyWithoutBrackets` = "[^\"\']+".r ^^ (_.toString)

  def `anyWithCompare` = "\\S+\\s?[=<>]".r ^^ (_.toString)

  def `anyWithCompareAndAnyWithOrWithoutBrackets` = `anyWithCompare` ~ "\"\'".r ~ `anyWithoutBrackets` ~ "\"\'".r ^^ {
    case col ~ br ~ body ~ bra => {
      val tempStr = s"'${col}\'${body}\''"
      bufferIndexLog += tempStr
      tempStr
    }
  }

  def `universalThreeSentence`: Parser[String] = "\\S+\\s?[=<>]\\s?[\"\']?\\S+".r ^^ { str => {
    parse(`index=log`, str) match {
      case Success(result, _) => {
        bufferIndexLog += result
        result
      }
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }}
  //def `colWithoutBrackets` = "\\S+\\s?[=<>]".r ~ "\"\'".r ~ "[^\"\']".r ~ "\"\'".r ^^ (_.toString)

  def `index`: Parser[String] = "index\\s?=".r //^^ (_ => "index=") //cases expression with index

  def `indexUniversal`: Parser[String] = "index\\s?=\\s?\\S+".r ^^ { str => {
    parse(`index=log`, str) match {
      case Success(result, _) => {
        bufferIndexLog += result
        result
      }
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }}

  def `colUniversal`: Parser[String] = "\\S+\\s?=\\s?\\S+".r ^^ { str => {
    parse(`index=log`, str) match {
      case Success(result, _) => {
        bufferIndexLog += result
        result
      }
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }}

  //cases expression with other simple expressions
  def `anySentenceForSeqParser` = "\\S+".r ^^ (str => {
    val tempValue = if (str == "NOT") {
      s"!("
    }
    else if (str == "GET" || str == "POST") {
      s"_raw like \\\'%$str%\\\'"
    }
    else s"$str"
    bufferOtherSentence += tempValue
    str
  })

  //`anySeqString` |
  def commonSentenceParser =
    `indexUniversal` |
      `anyWithCompareAndAnyWithOrWithoutBrackets` |
      `anySentenceForSeqParser`
  /*  ____________________________________________________________  */
  //Parsers for every phrases

  def commonSentence = rep1(commonSentenceParser) ^^ {
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
        } else if ((bufferOtherSentence(x).contains("GET") || bufferOtherSentence(x).contains("POST")) && x != 0) {
          if (bufferOtherSentence(x - 1) != "AND" && bufferOtherSentence(x - 1) != "OR") bufferOtherSentence(x) = s"AND ${bufferOtherSentence(x)}"
        }
      }
      if (i != -1) bufferOtherSentence.remove(i)
      //bufferOtherSentence.map(_.replaceAll("\"", "\\\'"))
      println(bufferIndexLog.mkString(" "))
      println(bufferOtherSentence.mkString(" "))
      println(bufferOtherSentence.mkString("").split("").size) //.split(" ").mkString(" "))
      val resBufferOtherSentence =  bufferOtherSentence.mkString(" ").replaceAll("\"","\\\\'")
      IndexApacheLog(bufferIndexLog, resBufferOtherSentence) //.toString//.mkString(" ").toString
  }

  case class IndexApacheLog(bufferIndexLogs: mutable.Buffer[String], groupCol: String) {
    val apBuffer = bufferIndexLogs.map(_.stripPrefix("\"").stripSuffix("\""))
    val gColl = groupCol.trim
    val grColl = if (gColl.trim != "GET") gColl else s"_raw like \\\'%$gColl%\\\'"

    override def toString: String = apBuffer.map(ap => s"```{'$ap' : { 'query': '$grColl', tws: 0, twf: 0}}```").mkString(",")
  }

  case class ParserHelper(str: String) {
    override def toString: String = s"$str"
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
    val str = "index=test GET POST"
    val simpleParser = new SimpleParser
    println(simpleParser.parseResult(simpleParser.commonSentence, str))
  }
}