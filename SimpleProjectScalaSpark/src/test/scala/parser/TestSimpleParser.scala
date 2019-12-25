package parser

import org.scalatest.FunSuite

class TestSimpleParser extends FunSuite {

  test("Check methods for SimpleParser class  for case test 1") {
    val simpleParser = new SimpleParser
    val str = "index=test Колонка=Test"
    val compareStr = "```{'test' : { 'query': 'Колонка=Test', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 2") {
    val simpleParser = new SimpleParser
    val str = "index=test"
    val compareStr = "```{'test' : { 'query': '', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 3") {
    val simpleParser = new SimpleParser
    val str = "index=\"test\""
    val compareStr = "```{'test' : { 'query': '', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 4") {
    val simpleParser = new SimpleParser
    val str = "index= test"
    val compareStr = "```{'test' : { 'query': '', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 5") {
    val simpleParser = new SimpleParser
    val str = "index=Тестовый_Индекс"
    val compareStr = "```{'Тестовый_Индекс' : { 'query': '', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 6") {
    val simpleParser = new SimpleParser
    val str = "index=test col1=20"
    val compareStr = "```{'test' : { 'query': 'col1=20', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 7") {
    val simpleParser = new SimpleParser
    val str = "index=test NOT col1=20"
    val compareStr = "```{'test' : { 'query': '!(col1=20)', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 8") {
    val simpleParser = new SimpleParser
    val str = "index=test col1=20 OR col2>30"
    val compareStr = "```{'test' : { 'query': 'col1=20 OR col2>30', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 9") {
    val simpleParser = new SimpleParser
    val str = "index=test (col1=20 AND col3=40) OR col2=30"
    val compareStr = "```{'test' : { 'query': '(col1=20 AND col3=40) OR col2=30', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 10") {
    val simpleParser = new SimpleParser
    val str = "index=test col1=\"20\""
    val compareStr = "```{'test' : { 'query': 'col1=\\\'20\\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 11") {
    val simpleParser = new SimpleParser
    val str = "index=test col1=\"test value\""
    val compareStr = "```{'test' : { 'query': 'col1=\\\'test value\\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 12") {
    val simpleParser = new SimpleParser
    val str = "index=test GET"
    val compareStr = "```{'test' : { 'query': '_raw like \\'%GET%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 13") {
    val simpleParser = new SimpleParser
    val str = "index=test GET POST"
    val compareStr = "```{'test' : { 'query': '_raw like \\'%GET%\\' AND _raw like \\'%POST%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 14") {
    val simpleParser = new SimpleParser
    val str = "index=test GET AND POST"
    val compareStr = "```{'test' : { 'query': '_raw like \\'%GET%\\' AND _raw like \\'%POST%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 15") {
    val simpleParser = new SimpleParser
    val str = "index=test col1=20 GET"
    val compareStr = "```{'test' : { 'query': 'col1=20 AND _raw like \\'%GET%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 16") {
    val simpleParser = new SimpleParser
    val str = "index=test col1=20 AND GET"
    val compareStr = "```{'test' : { 'query': 'col1=20 AND _raw like \\'%GET%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 17") {
    val simpleParser = new SimpleParser
    val str = "index=test col1=20 OR GET OR POST"
    val compareStr = "```{'test' : { 'query': 'col1=20 OR _raw like \\'%GET%\\' OR _raw like \\'%POST%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 18") {
    val simpleParser = new SimpleParser
    val str = "index=test \"русские trololo;? символы\""
    val compareStr = "```{'test' : { 'query': '_raw like \\\'%русские trololo;? символы%\\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
    }

  test("Check methods for SimpleParser class for case test 19") {
    val simpleParser = new SimpleParser
    val str = "index=test \"русские символы\" OR \"слово\"  POST"
    val compareStr = "```{'test' : { 'query': '_raw like \\'%русские символы%\\' OR _raw like \\'%слово%\\' AND _raw like \\'%POST%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }
  test("Check methods for SimpleParser class for case test 20") {
    val simpleParser = new SimpleParser
    val str = "index=test \"русские \\\"символы\\\" host\" OR \"слово\"  POST"
    val compareStr = "```{'test' : { 'query': '_raw like \\'%русские \\\"символы\\\" host%\\' OR _raw like \\'%слово%\\' AND _raw like \\'%POST%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 21") {
    val simpleParser = new SimpleParser
    val str = "index=test GET* AND POST*"
    val compareStr = "```{'test' : { 'query': '_raw rlike \\'GET.*\\' AND _raw rlike \\'POST.*\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 22") {
    val simpleParser = new SimpleParser
    val str = "index=test *G*ET* AND PO*ST*"
    val compareStr = "```{'test' : { 'query': '_raw rlike \\'.*G.*ET.*\\' AND _raw rlike \\'PO.*ST.*\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 23") {
    val simpleParser = new SimpleParser
    val str = "index=test col1=20* AND GET"
    val compareStr = "```{'test' : { 'query': 'col1 rlike \\'20.*\\' AND _raw like \\'%GET%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 24") {
    val simpleParser = new SimpleParser
    val str = "index=test col1=\"20*\" AND GET"
    val compareStr = "```{'test' : { 'query': 'col1 rlike \\'20.*\\' AND _raw like \\'%GET%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 25") {
    val simpleParser = new SimpleParser
    val str = "index=test col1=20 OR GET OR POST OR col2=20 OR col3=20 OR col4=20 OR col5=20 OR col6=20 OR col7=20"
    val compareStr = "```{'test' : { 'query': 'col1=20 OR _raw like \\'%GET%\\' OR _raw like \\'%POST%\\' OR col2=20 OR col3=20 OR col4=20 OR col5=20 OR col6=20 OR col7=20', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 26") {
    val simpleParser = new SimpleParser
    val str = "index=test col1=\"20\\*\" AND GET"
    val compareStr = "```{'test' : { 'query': 'col1='20*' AND _raw like \\'%GET%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 27") {
    val simpleParser = new SimpleParser
    val str = "GET AND POST index=test"
    val compareStr = "```{'test' : { 'query': '_raw like \\'%GET%\\' AND _raw like \\'%POST%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 28") {
    val simpleParser = new SimpleParser
    val str = "index=test index=test2 col1=20"
    val compareStr = "```{'test' : { 'query': 'col1=20', tws: 0, twf: 0}}```,```{'test2' : { 'query': 'col1=20', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.commonSentenceTwoVariant, str)
    println(testStr)
    assert(testStr == compareStr)
  }
}
