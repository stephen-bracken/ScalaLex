package lexerGenerator

import scala.annotation.tailrec

object LexerFactory{
    type Section = (List[GeneratorToken],Boolean)
    private var o:Output = null
    private var rawdefs:List[GeneratorToken] = Nil
    private var rawrules:List[GeneratorToken] = Nil
    private var rawroutines:List[GeneratorToken] = Nil
    private var withdefs = false
    private var withrules = false
    private var withroutines = false
    class Output(l: List[GeneratorToken]*){
        private var id = 0
        /** gets the function id for a regex */
        private var idMap:Map[(String,String),String] = Map()
        private var regexes:List[(String,String)] = Nil
        private var in = l.toList
        private val sb: StringBuilder = StringBuilder.newBuilder
        private var defs:Map[String,String] = Map()
        private var rules:List[String] = Nil
        private var states:List[String] = Nil
        private var inclusive:Boolean = false
        //begin class
        sb.append("class Lex {\n")
        if(withdefs){processDefs(in.head);in = in.tail}
        //add state variables
        sb.append("private var state = \"INITIAL\"\nprivate val states:List[String] = List(\"INITIAL\",")
        sb.append(states.reduceLeft((a,b) => '"'+a+'"'+','+'"'+b+'"'))
        sb.append(")\n")
        sb.append("private val inclusive:Boolean = "+inclusive+'\n')
        if(withrules){processRules(in.head); in = in.tail}
        if(withroutines){processRoutines(in.head.head.asInstanceOf[CodeBlock]);in = in.tail}
        sb.append('}')
        /** gets the definitions, options, code and states from the defs section */
        private def processDefs(d: List[GeneratorToken]) = {
            @tailrec
            def processDef(l: List[GeneratorToken]):Unit = l match {
                case Nil => {}
                case Definition(i,r)::xs => {defs = defs.updated(i(),r());processDef(xs)}
                case Declaration(s)::xs => {setOption(Util.asString(s));processDef(xs)}
                case LexingState(s,i)::xs => {states = s; inclusive = i; processDef(xs)}
                case CodeBlock(c)::xs => {sb.append(Util.asString(c));processDef(xs)}
                case Comment(s)::xs => processDef(xs)
                case Identifier(s)::xs => throw new LexerOutputError("Unclosed identifier found: " + Util.asString(s))
                case LexingRule(s,r,c)::xs => throw new LexerOutputError("Rule found in defs section: " + s + r)  
                case x::xs => throw new LexerOutputError("Unexpected expression in defs: " + x)
            }
            processDef(d)
        }
        /** compiles the rules from the rules section into methods for yylex to call */
        private def processRules(r: List[GeneratorToken]) = {
            @tailrec
            def processRule(l: List[GeneratorToken]):Unit = l match {
                case Nil => {}
                case LexingRule(s,r,c)::xs => {
                    val reg = lookupDefs(r())
                    regexes = addRegex(s(),reg)
                    val rb = StringBuilder.newBuilder
                    rb.append("def " + getId(s(),reg) + "() = {")
                    rb.append(c())
                    rb.append("}\n")
                    rules = rules :+ rb.mkString
                    processRule(xs)
                }
                case Comment(c)::xs => processRule(xs)
                case x::xs => throw new LexerOutputError("Unexpected token in rules section: " + x)
            }
            processRule(r)
            sb.append("//### RULES ###")
            rules.map(s => sb.append("\n"+s))
        }
        /** adds the code from the routines section */
        private def processRoutines(c: CodeBlock) = {
            sb.append("//### USER SUBROUTINES ###")
            sb.append(c())
        }
        //######Defs functions#######
        private def setOption(o: String) = {}
        //######Rules functions######
        /** assigns a regex function to a given id */
        private def getId(s: String,r: String):String = {
            val i = "rule" + id
            idMap = idMap.updated((s,r),i)
            id += 1
            i
        }
        private def addState(s: String):List[String] = {
            if(states.contains(s)) {throw new LexerOutputError("Duplicate state declaration: " + s)}
            states :+ s
        }
        private def addRegex(s: String,r: String):List[(String,String)] = {
            if(regexes.contains(r)) {throw new LexerOutputError("Duplicate regex declaration: " + r)}
            regexes :+ (s,r)
        }
        /** looks up and replaces the regex value of names from the defs*/
        private def lookupDefs(s: String):String = {
            var n = s
            val r = (for{
                i <- 0 to s.length
                e <- i to s.length
            } yield {
                s.substring(i,e)}).filter(p => defs.keySet.contains(p))
                if(!r.isEmpty){for (i <- r) yield{n = n.replace(i,defs(i))}}
                n
            }
        /** returns the output string */
        def apply() = {
            sb.mkString
        }
    }
    /** adds the tokens from the defs section to the output file */
    def withDefs(l: Section):Unit = {
        rawdefs = l._1
        withdefs = l._2
    }
    /** adds the tokens from the rules section to the output file */
    def withRules(l: Section):Unit = {
        rawrules = l._1
        withrules = l._2
    }
    /** adds the tokens from the routines section to the output file */
    def withRoutines(l: Section):Unit = {
        rawroutines = l._1
        withroutines = l._2
    }
    /** produces the output file text */
    def makeLexer = {
        var l:List[List[GeneratorToken]] = Nil
        if(withdefs){l = l :+ rawdefs}
        if(withrules){l = l :+ rawrules}
        if(withroutines){l = l :+ rawroutines}
        val o = new Output(l:_*)
        o()
    }
}

class LexerOutputError(msg: String) extends Error(msg)