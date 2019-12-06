package parser

import scala.collection.mutable
import scala.util.parsing.combinator._

class SimpleParser extends RegexParsers {
  //terminals  [\s]$
  def `index`: Parser[String] = "index=".r //^^ (_ => "index=") //cases expression with index

  def `any[=><]`: Parser[String] = "[0-9a-zA-Zа-яА-ЯЁё_]+\\s?[><=]\\s?".r //^^ (_ => "index=") //cases expression with index

  def anySymbols: Parser[String] = "[0-9a-zA-Zа-яА-ЯЁё_><=]+".r //^^ (_ => "index=") //cases expression with index

  def `index=log` = `index` ~> commonSimpleString ^^ (_.toString)

  def repBoolAndString = booleanExpression ~ commonSimpleString.?

  def anySentence = ".+".r ^^ (_.toString)

  def anyAndBooleanOrAnyExpression = anySentence //| anyAndBooleanExpression //^^ (_.toString)

  def anyWithoutBool = "[^NOT|OR|AND]+".r ^^ (_.toString)

  def commonSimpleString = "\\S+\\s?".r ^^ (_.toString)

  def bracketRight = "\\(".r ^^ (_.toString)

  def bracketLeft = "\\)".r ^^ (_.toString)

  // var listSimpleParserExpression = List[Parser[String]](`index`, `any[=><]`, anySymbols, `index=log`, GETORPOST, booleanExpression)
  var bufferPreRez = mutable.Buffer[String]()
  var bufferIndexLog = mutable.Buffer[String]()
  var bufferOtherSentence = mutable.Buffer[String]()
  var resBuffer = mutable.Buffer[String]()
  var bufferSimpleStringsBool = mutable.Buffer[String]()
  var bufferStringsBool = mutable.Buffer[String]()

  def repRezSentence = rep1(commonSimpleString)

  lazy val GETORPOST = "GET|POST".r

  lazy val booleanExpression = "NOT|OR|AND".r //^^ (_.toString)

  lazy val compareSymbols = "=><".r //^^ (_.toString)

  lazy val `indexAny` = "index|index[0-9a-zA-Zа-яА-ЯЁё_><=]+".r //^^ (_.toString)

  lazy val `indexOne` = "index".r //^^ (_.toString)

  var i = 0

  var j = 0
  /*
    def worker(str: String) = {
      val mass = str.split(" ")
      mass.foreach{
        case str@ (`index` | `indexAny`()) => s" $str"
        case boolValue@ booleanExpression() => if (boolValue == "NOT") s" !(" else s" ${boolValue}"
        case boolValue@ compareSymbols() => s"${boolValue.trim}"
      }

    }

   */

  def regFirstPreMatcher(value: String): String = value match {
    case booleanExpression() => if (value == "NOT") s" !(" else s" ${value}"
    //case compareSymbols() => s"${value.trim}"
    case `indexAny`() => s" ${value}"
    case GETORPOST() => s"'_raw like \\'%${value.trim}%\\'"
    case str => {
      i += 1
      s"${str.trim}"
    }
  }

  def commonSentence = rep1(commonSimpleString) ^^ {
    listSentence =>
      listSentence.foreach { value =>
        val sar = regFirstPreMatcher(value.trim)
        bufferPreRez += sar
      }
      bufferPreRez.mkString("").split(" ").foreach {
        case str@`indexAny`() => {
          val res = this.parseHelper(indexParser, str)
          if (res != "") bufferIndexLog += res
        }
        case str => bufferOtherSentence += str.trim
      }
      println(i) //.split(" ").mkString(" "))
      println(bufferPreRez.mkString("")) //.split(" ").mkString(" "))
      //println(bufferPreRez.size) //.split(" ").mkString(" "))
      //println(bufferOtherSentence.mkString(" ")) //.split(" ").mkString(" "))
      println(bufferIndexLog.mkString(" ")) //.split(" ").mkString(" "))
      println(bufferIndexLog.size) //.split(" ").mkString(" "))
      //println(bufferOtherSentence.size) //.split(" ").mkString(" "))

      IndexApacheLog(bufferIndexLog, bufferOtherSentence.map(_.trim).mkString(" ")) //.toString//.mkString(" ").toString
  }

  def commonSent(parserString: Parser[String]) = parserString ^^ (anyExpression => ParserHelper(anyExpression.toString))

  def indexParser = `index` ~> anySymbols ^^ (anyExpression => ParserHelper(anyExpression.toString))

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
   * This method a compared res and st, if it is equal => return Success
   *
   * @param res
   * @param str
   * @return
   */
  def parseHelper(res: Parser[ParserHelper], str: String): String = {
    parse(res, str) match {
      case Success(matched, _) => matched.toString
      case Failure(_, _) => s""
      case Error(_, _) => s""
    }
  }

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
    val str = "index= test index = test1 NOT AND ( dgg ) GET"
    val simpleParser = new SimpleParser
    println(simpleParser.parseResult(simpleParser.commonSentence, str))
  }
}

/*

 def anyStringValueWithoutBrackets: Parser[String] = "\\s".? ~> "[0-9A-Za-zА-Яа-яЁё=<>_]+".r ^^ ( _.trim )

 def anyStringValueWithOrWithoutBrackets: Parser[String] = {
   anyStringValueWithoutBrackets |
   "\"" ~> anyStringValueWithoutBrackets <~ "\"" |
   anyStringValueWithoutBrackets ~ "\\s" ~ anyStringValueWithoutBrackets ^^ (_.toString)
 }
 def `col=`: Parser[String] = "[A-Za-zА-Яа-яЁё]+[1-9][=><]+".r ^^ (_.toString)

 def `col=20`: Parser[String] = `col=` ~ "\"".? ~ anyStringValueWithOrWithoutBrackets ~ "\"".? ^^ {
   case col ~ brRight ~ number ~ brLeft => {
     if ((brRight.fold("")(_.toString) != "")  && (brLeft.fold("")(_.toString) != "")) {
     s"$col\\'$number\\'"
     } else s"$col$number"
   }
 }
 def colOrRaw: Parser[String] = `col=20` | GETORPOST ^^ {_.toString}
 def groupCol = booleanExpression.? ~ "(".? ~ `col=20`.? ~ booleanExpression.? ~ `col=20`.? ~ ")".? ~ booleanExpression.? ~ `col=20`.? ^^ {
   case not ~ br1 ~ col1 ~ and_or1 ~ col2 ~ br2 ~ and_or2 ~ col3 => {
     val resStringMass = List(br1,
       col1,
       and_or1 match {
         case Some(x) => Some(s" $x ")
         case None => None
       },
       col2,
       br2,
       and_or2 match {
         case Some(x) => Some(s" $x ")
         case None => None
       },
       col3).map(_.fold("")(str => s"${str}")).mkString("")
     val notOpt = not.fold("")(str => s"$str")
     if (notOpt != "") s"!($resStringMass)"
     else s"$resStringMass"
   }
 }

 def resIndex = `index=` ~> anyStringValueWithOrWithoutBrackets ~ groupCol.? ~ anyStringValueWithOrWithoutBrackets.? ^^ {//~ anyStringValueWithOrWithoutBrackets.? ~ colOrRaw.? ~ groupCol.? ^^ {
   case ap ~ grCol ~ anyStr => {
     val rCol = grCol.fold("")(str => s"$str").trim
     val aStr = anyStr.fold("")(str => s"$str").trim
     val grColRez = if (rCol != "") {
       rCol
     } else if (aStr != "") {
       aStr
     } else ""
     IndexApacheLog(ap, grColRez) }
 }
*/