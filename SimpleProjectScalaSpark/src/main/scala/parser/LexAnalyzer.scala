package parser

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator._

class LexAnalyzer extends RegexParsers {
    //terminals  [\s]$
    def `index=`: Parser[String] = "index\\s?=\\s?".r ^^ (_ => "index=") //cases expression with index
    def anyStringValueWithoutBrackets: Parser[String] = "\\s".? ~> "[0-9A-Za-zА-Яа-яЁё=<>_]+".r ^^ ( _.trim )
    def anyStringValueWithOrWithoutBrackets: Parser[String] = {
      anyStringValueWithoutBrackets |
        "\"" ~> anyStringValueWithoutBrackets <~ "\"" |
        anyStringValueWithoutBrackets ~ "\\s" ~ anyStringValueWithoutBrackets ^^ (_.toString)
    }
    def booleanExpression: Parser[String] = "[NOT|OR|AND]".r ^^ (_.toString)
    def `col=`: Parser[String] = "[A-Za-zА-Яа-яЁё]+[1-9][=><]+".r ^^ (_.toString)
    def `col=20`: Parser[String] = `col=` ~ "\"".? ~ anyStringValueWithOrWithoutBrackets ~ "\"".? ^^ {
      case col ~ brRight ~ number ~ brLeft => {
        if ((brRight.fold("")(_.toString) != "")  && (brLeft.fold("")(_.toString) != "")) {
          s"$col\\'$number\\'"
        } else s"$col$number"
      }
    }
    def withBoolExpression = booleanExpression ~ anyStringValueWithOrWithoutBrackets ^^ {
      case bool ~ str => if (bool == "NOT") {
        s"!($str)"
      } else (bool == "")
    }

    def GETORPOST: Parser[String] = "[GET][POST]".r ^^ {_.toString}
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

    case class IndexApacheLog( apache: String, groupCol: String) {
      val ap = apache.stripPrefix("\"").stripSuffix("\"")
      val gColl = groupCol.trim
      val grColl = if (gColl.trim != "GET") gColl else s"_raw like \\\'%$gColl%\\\'"
      override def toString: String = s"```{'$ap' : { 'query': '$grColl', tws: 0, twf: 0}}```"
    }

    def parseResult(resIndex: Parser[IndexApacheLog], str: String): String = {
      parse(resIndex, str) match {
        case Success(matched, _) => matched.toString
        case Failure(msg, _) => s"FAILURE  + $msg"
        case Error(msg, _) => s"Error + $msg"
      }
    }
  }
/*
  object ObjectSimpleParser extends SimpleParser {
    def main(args: Array[String]): Unit = {
      val  str  = "index=test"
      println(parseResult(resIndex, str))
    }
  }

 */