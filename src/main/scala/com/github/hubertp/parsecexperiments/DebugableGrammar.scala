package com.github.hubertp.parserexperiments

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

import scala.util.parsing.combinator.debugging

object DebugableGrammar extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "{", "}", ",", "*", "+")
  lexical.reserved   ++= List("true", "false", "succ",
                              "pred", "iszero", "zero", "Mip", "Mup", "Map", "Blip", "Blop", "Blap", "Blup")
  import debugging.ParserMacros._  
  
  def Term(implicit loc0: debugging.ParserLocation): Parser[Term] = (
    // BoolTerm

    "Blip" ~ ("Blup" | "Blop") ~ "Blap" ~ "Blap" ~ "Blup" ^^^ True
    | "Blip" ~ bopbop ~ "Blap" ~ "Blap" ~ "Blip" ^^^ True
    //| rep("Blip") ~ "Blop" ~ rep("Blap") ~ "Blip" ^^^ False
    | "Blip" ~ tjah ~ "Blap" ~ "Blap" ^^^ True
    | SimpleChurchNumTerm
    | IsZeroTerm
  )

  def bopbop(implicit loc0: debugging.ParserLocation) : Parser[Term] = (
      "Mip" ^^^ True
    | "Mup" ^^^ True
    | "Map" ^^^ True
  )

  def tjah(implicit loc0: debugging.ParserLocation) : Parser[Term] = "Blop" ~ "Blap" ^^^ False

  def IsZeroTerm(implicit loc0: debugging.ParserLocation): Parser[Term] = (
    "iszero" ~> SimpleChurchNumTerm ^^ (t => IsZero(t))
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
    //val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    val tokens = new lexical.Scanner("Blip Blop Blap Blap Blap")
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
