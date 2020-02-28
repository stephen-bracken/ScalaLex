package lexerGenerator

import scala.io.Source
import com.typesafe.scalalogging.LazyLogging
import scala.collection.immutable.Nil
import scala.annotation.tailrec
class Lexer(input: List[GeneratorToken],private var _in:String) extends LazyLogging{
    private var _out:List[Char] = Nil
    private var state = "INITIAL"
    private var defs:Map[String,String] = makeDefs(getToken(input,Nil,0).asInstanceOf[List[Definition]])
    private var options:List[Declaration] = getToken(input,Nil,1).asInstanceOf[List[Declaration]]
    private var rules:List[LexRule] = makeRules(getToken(input,Nil,2).asInstanceOf[List[LexingRule]])
    private def makeDefs(l: List[Definition]):Map[String,String] = {
        (for(d <- l) yield {(d.i.toString -> d.r.toString)}).toMap
    }
    private def lookupDefs(s: String):String = {
        var n = s
        val r = (for{
            i <- 0 to s.length
            e <- i to s.length
        } yield {s.substring(i,e)}).filter(p => defs.keySet.contains(p))
        if(!r.isEmpty){for (i <- r) yield{n = n.replace(i,defs(i))}}
        n
    }
    private def makeRules(l: List[LexingRule]) = {
        def makeRule(lx: LexingRule):LexRule = {
            new LexRule(lx)
        }
        for(lx <- l) yield (makeRule(lx))
    }
    @tailrec
    private def getToken(l: List[GeneratorToken],a: List[GeneratorToken],i: Int):List[GeneratorToken] = l match {
        case Nil => a
        case x@Definition(id,r) :: xs if i == 0 =>
            getToken(xs,a :+ x.head,i)
        case x@Declaration(o)::xs if i == 1 =>
            getToken(xs,a :+ x.head,i)
        case x@LexingRule(s,r,c) :: xs if i == 2 =>
            getToken(xs,a :+ x.head,i)
        case x :: xs =>
            getToken(xs,a,i)
    }
    /** performs lexical analysis on the input file */
    def yylex = {
        
        val bufferedSource = Source.fromFile(_in)
        var lines:List[String] = List()
        for (line <- bufferedSource.getLines) {
            lines = line :: lines
            logger.trace(line)
        }
        bufferedSource.close
        lines = lines.reverse
        var inputSeq = lines.flatten
        var _c:Char = inputSeq.head
        //IO methods
        /** gets the next character fron the input stream */
        def input = _c
        /** writes a character to the output stream */
        def output(c: Char) = {_out = c::_out}
        /** writes a character to the input stream */
        def unput(c: Char) = {inputSeq = c::inputSeq}
    }
    def getMatch(s: String) = {
        var length = 0
        var matched = ""
        var rule:LexRule = null
        def findMatches(l: List[LexRule]):(LexRule,Int,String) = l match {
            case Nil => 
                (rule,length,matched)
            case x::xs if stateMatch(x.startCondition) =>
                val r = x.parse(s)
                if(r._1 > length) {rule = x; length = r._1; matched = r._2}
                findMatches(l.tail)
        }
        findMatches(rules.filter(r => r.startCondition == state))
    }
    def stateMatch(s: String) = s match {
        case "" if state == "INITIAL" =>
            true
        case x =>
            state == x
    }

    //private var _c:Char = _in.head


    
    //def reject

}

class Token(val name:String, val args:List[Any]) {
}

class LexerError(msg: String) extends Error(msg){}