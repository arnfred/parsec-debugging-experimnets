package com.github.hubertp.parserexperiments

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

import scala.util.parsing.combinator.debugging

object DebugableGrammar extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "{", "}", ",", "*", "+")
  lexical.reserved   ++= List("true", "false", "succ",
                              "pred", "iszero", "zero")
  import debugging.ParserMacros._  
  
  def Term(implicit loc0: debugging.ParserLocation): Parser[Term] = (
    BoolTerm
    | SimpleChurchNumTerm
  )
//    | failure("illegal start of simple term"))
  
  def BoolTerm(implicit loc0: debugging.ParserLocation): Parser[Term] = (
    "true"   ^^^ True
    | "false"  ^^^ False
  )
  
  def SimpleChurchNumTerm(implicit loc: debugging.ParserLocation): Parser[Term] = (
    "zero"   ^^^ Zero
    | "succ" ~ SimpleChurchNumTerm ^^ { case "succ" ~ t => Succ(t) }
  )
  
  def main(args: Array[String]) {
    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    //val tokens = new lexical.Scanner("succ succ succ zero")
    val mainParser = phrase(Term)
    mainParser(tokens) match {
      case Success(trees, _) =>
        try {
          println("Parsed as: " + trees)
        } catch {
          case tperror => println(tperror.toString)
        }
      case e =>
        println(e)
    }
  }
}
