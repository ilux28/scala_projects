package parser

import org.scalatest.FunSuite

class TestSimpleParser extends FunSuite {
  val simpleParser = new SimpleParser
/*
  test("Check methods for SimpleParser class  for case test 1") {
    val  str  = "index=test Колонка=Test"
    val compareStr = "```{'test' : { 'query': 'Колонка=Test', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 2") {
    val  str  = "index=test"
    val compareStr = "```{'test' : { 'query': '', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 3") {
    val  str  = "index=\"test\""
    val compareStr = "```{'test' : { 'query': '', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 4") {
    val  str  = "index= test"
    val compareStr = "```{'test' : { 'query': '', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 5") {
    val  str  = "index=Тестовый_Индекс"
    val compareStr = "```{'Тестовый_Индекс' : { 'query': '', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 6") {
    val  str  = "index=test col1=20"
    val compareStr = "```{'test' : { 'query': 'col1=20', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 7") {
    val  str  = "index=test NOT col1=20"
    val compareStr = "```{'test' : { 'query': '!(col1=20)', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 8") {
    val  str  = "index=test col1=20 OR col2>30"
    val compareStr = "```{'test' : { 'query': 'col1=20 OR col2>30', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 9") {
    val  str  = "index=test (col1=20 AND col3=40) OR col2=30"
    val compareStr = "```{'test' : { 'query': '(col1=20 AND col3=40) OR col2=30', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 10") {
    val  str  = "index=test col1=\"20\""
    val compareStr = "```{'test' : { 'query': 'col1=\\\'20\\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 11") {
    val  str  = "index=test col1=\"test value\""
    val compareStr = "```{'test' : { 'query': 'col1=\\\'test value\\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 12") {
    val  str  = "index=test GET"
    val compareStr = "```{'test' : { 'query': '_raw like \\'%GET%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 13") {
    val  str  = "index=test GET"
    val compareStr = "```{'test' : { 'query': '_raw like \\'%GET%\\'', tws: 0, twf: 0}}```"
    val testStr = simpleParser.parseResult(simpleParser.resIndex, str)
    println(testStr)
    assert(testStr == compareStr)
  }
 */

}
