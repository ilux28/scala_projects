package parser

import scala.collection.mutable
import scala.util.parsing.combinator._

class SimpleParser extends RegexParsers {
  //terminals  [\s]$
  def `index`: Parser[String] = "index=".r //^^ (_ => "index=") //cases expression with index

  def `any[=><]`: Parser[String] = "[0-9a-zA-Zа-яА-ЯЁё_]+\\s?[><=]\\s?".r //^^ (_ => "index=") //cases expression with index

  def anySymbols: Parser[String] = "[0-9a-zA-Zа-яА-ЯЁё_><=]+".r //^^ (_ => "index=") //cases expression with index

  def `index=log`= `index` ~> commonSimpleString ^^ (_.toString)

  def repBoolAndString = booleanExpression ~ commonSimpleString.?

  def GETORPOST: Parser[String] = "[GET][POST]".r

  def anySentence = ".+".r ^^ (_.toString)

  def anyAndBooleanOrAnyExpression = anySentence //| anyAndBooleanExpression //^^ (_.toString)

  def anyWithoutBool = "[^NOT|OR|AND]+".r ^^ (_.toString)

  lazy val booleanExpression = "NOT|OR|AND".r //^^ (_.toString)

  def commonSimpleString = "\\S+\\s?".r ^^ (_.toString)

  def bracketRight = "\\(".r ^^ (_.toString)

  def bracketLeft = "\\)".r ^^ (_.toString)
 
  var listSimpleParserExpression = List[Parser[String]](`index`, `any[=><]`, anySymbols, `index=log`, GETORPOST, booleanExpression)
  var bufferIndexLogs = mutable.Buffer[String]()
  var resBuffer = mutable.Buffer[String]()
  var bufferSimpleStringsBool = mutable.Buffer[String]()
  var bufferStringsBool = mutable.Buffer[String]()

  def repRezSentence = rep1(commonSimpleString)
/*
  def successVal(param: Parser[String], str: String) = {
    val parser = parseHelper(commonSent(param), str)
    if ( parser != "") parser else ""
  }

 */
  def regMatcher(value: String) : String = value match {
    case booleanExpression() => if (value == "NOT") s"!(" else value.trim
    case booleanExpression() => if (value == "NOT") s"!(" else value.trim
  }

  def commonSentence = rep1(commonSimpleString) ^^ {
    listSentence =>
      listSentence.foreach { value =>
        //successVal(booleanExpression, value)
        /*
        val sar = if (value == "NOT") {
          s"!("
        } else value.trim
        value match {
          case booleanExpression() => if (value == "NOT") s"!(" else value.trim
          case booleanExpression() => if (value == "NOT") s"!(" else value.trim
        }
        val sar = regMatcher(value)
        if (sar != "") bufferIndexLogs += sar
        */
        println(bufferIndexLogs.mkString(""))
        println(bufferIndexLogs.size)
        IndexApacheLog(bufferIndexLogs, bufferSimpleStringsBool.map(_.trim).mkString(" ")) //.toString//.mkString(" ").toString
      }
  }

  def commonSent(parserString: Parser[String]) = parserString ^^ (anyExpression => ParserHelper(anyExpression.toString))

  case class IndexApacheLog( bufferIndexLogs: mutable.Buffer[String], groupCol: String) {
    val apBuffer = bufferIndexLogs.map(_.stripPrefix("\"").stripSuffix("\""))
    val gColl = groupCol.trim
    val grColl = if (gColl.trim != "GET") gColl else s"_raw like \\\'%$gColl%\\\'"
    override def toString: String = apBuffer.map(ap =>  s"```{'$ap' : { 'query': '$grColl', tws: 0, twf: 0}}```").mkString(",")
  }

  case class ParserHelper(str: String) {
    override def toString: String = s"$str"
  }

  def parseHelper(res: Parser[ParserHelper], str: String): String = {
    parse(res, str) match {
      case Success(matched, _) => matched.toString
      case Failure(_ , _) => s""
      case Error(_ , _) => s""
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
    val  str  = "index = test AND dgg"
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