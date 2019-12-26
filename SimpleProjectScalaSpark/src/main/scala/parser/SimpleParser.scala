package parser

import scala.collection.mutable
import scala.util.parsing.combinator._

class SimpleParser extends RegexParsers {

  /***
   * Вспомогательные массивы, обработка которых, пойдет в результат
   */

  var bufferIndexLog = mutable.Buffer[String]()
  var bufferOtherSentence = mutable.Buffer[String]()

  /***
   * ^^ документируется как «парсер комбинатор для применения функции».
   * Если парсинг слева от ^^ успешен, выполняется функция справа.
   * Если метод `anyMethod` возвращает Parser типа String, то функция справа от ^^ должна возвращать String.
   */

  //-------------------------------------------------------------------//


  /***
   * Набор простейших элементов для парсинга - типа токены
   * И составленные из них более сложные методы - конструирование из токенов
   */

  def `anySentenceWithoutGap` = """[^"'\s]+""".r ^^ ( _.toString )

  def `anySentenceWithColWithoutGap` = """[^\s]+""".r ^^ ( _.toString )

  def `anySentenceWithoutGapElse` = """\S+""".r ^^ (_.toString)


  /**
   * Парсит выражения вида "index\\s?=\\s?\\S+".r
   * и закидывает положительный результат в bufferIndexLog,
   * отвечающий за количество выдаваемых json и содержания их заголовков
   */

  def `indexUniversal`: Parser[String] = "index\\s?=\\s?\\S+".r ^^ { str => {
    parse(`index=log`, str) match {
      case Success(result, _) => {
        bufferIndexLog += result
        result
      }
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }}

  def `index`: Parser[String] = "index\\s?=".r //^^ (_ => "index=") //cases expression with index

  def `index=log` = `index` ~> `anySentenceWithoutGapElse` ^^ (_.toString)

  /**
   * Для парсинга булевых выражений вида "AND|OR|NOT"
   */
  def `anyBooleanSentence` = "AND|OR|NOT".r ^^ { str => {
    val strRes = if (str == "NOT") s"!(" else str
    strRes
  } }

  /**
   * Для парсинга выражений-запросов вида "\\*?G\\*?E\\*?T\\*?+|\\*?P\\*?O\\*?S\\*?T\\*?+"
   */

  def `anyRequests` = "\\*?G\\*?E\\*?T\\*?+|\\*?P\\*?O\\*?S\\*?T\\*?+".r ^^ { str => {
    val strRequest = if (str.contains("*")) {
      val tempStr = str.replaceAll("""\*""", """.*""")
      s"_raw rlike \\'$tempStr\\'"
    } else {
      s"_raw like \\'%$str%\\'"
    }
    strRequest
  } }

  /**
   * Группа функций для парсинга различных выражений:
   * 1. Некоторого набора символов со знаком сравнения на конце, вида """\S+\s?[=<>]""".r
   * 2. Некоторого набора символов заключенных в двойных или одинарных кавычках, вида """["|'].+?["|']""".r
   * 3. Некоторого набора символов заключенных в двойных или одинарных кавычках не содержащего их внутри, вида """["|'][^\\"|'].+?[^\\"|']["|']""".r
   * 4. Некоторого набора символов начинающийся не с кавычек и заканчивающийся ими, вида """[^"|']+["|']""".r
   * 5. Некоторого набора символов начинающийся с кавычек и заканчивающийся не кавычками, вида """["|'][^"|'\s]+""".r
   */
  def `commonColWithCompare` = """\S+\s?[=<>]""".r ^^ (_.toString)

  def `lazyQuantifierWithBrackets` = """["|'].+?["|']""".r  ^^ { _.toString }

  def `lazyQuantifierInnerShieldedBrackets` = """["|'][^\\"|'].+?[^\\"|']["|']""".r ^^ { _.toString }

  def `sentenceWithRightBrackets` = """[^"|']+["|']""".r  ^^ {  _.toString  }

  def `sentenceWithLeftBrackets` = """["|'][^"|'\s]+""".r  ^^ { _.toString }

  /***
   * Метод для парсинга всех остальных выражений для возможных вариантов вида 'col1='20*'
   *
   * @return
   */

//TODO требует рефракторинга(скоро) 26.12.19

  def `sentenceWithCol` = `commonColWithCompare`.? ~ `anyVariantsWithBrackets` ^^ {
    case colOpt ~ anySent => {
      val col = colOpt.fold("")(_.toString)
      val resStr = if (col.contains("index")) {
        parse(`index=log`, s"$col$anySent") match {
          case Success(result, _) => {
            bufferIndexLog += result
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

  /***
   * Методы для парсинга выражений "высокого уровня"
   * (Обратите внимание на очередность - порядок имеет значение: сначала проверятся на соответствие первый, если соответствие
   * не найдено, проверяем следующий)
   */

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
   * Метод выдающий результат на вснове подготовленных данных из вспомогательных массивов
   */
      //TODO Возможно тоже требует рефракторинга 26.12.19

  def commonSentenceTwoVariant = rep1(`universalSentence`) ^^ {
    listSentence =>
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
      val resBufferOtherSentence =  bufferOtherSentence.map(_.trim).mkString(" ").trim//.replaceAll("\"","\\\\'")
      IndexApacheLog(bufferIndexLog, resBufferOtherSentence) //.toString//.mkString(" ").toString
  }

  /***
   * Case class в методе @toString формирующий результирующий JSON
   */

  case class IndexApacheLog(bufferIndexLogs: mutable.Buffer[String], groupCol: String) {
    val apBuffer = bufferIndexLogs.map(_.stripPrefix("\"").stripSuffix("\""))
    override def toString: String = apBuffer.map(ap => s"```{'$ap' : { 'query': '$groupCol', tws: 0, twf: 0}}```").mkString(",")
  }

  /**
  * This method a parsed res and str, if it is equal => return Success
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