package lexerGenerator

import scala.annotation.tailrec

object LexerFactory{
    type Section = (List[GeneratorToken],Boolean)
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
        private var in = l.toList
        private val sb: StringBuilder = StringBuilder.newBuilder
        /** (name -> regex) */
        private var defs:Map[String,String] = Map()
        private var rules:List[String] = Nil
        private var states:List[String] = Nil
        private var inclusive:Boolean = false
        sb.append("//######DEFINITIONS/OUTER EXPRESSIONS/IMPORTS######\n")
        sb.append("import scala.io.Source\nimport java.io.File\nimport java.io.BufferedWriter\nimport java.io.FileWriter\n")
        if(withdefs){processDefs(in.head);in = in.tail}
        //begin class
        sb.append("class Lex {\n")
        //states
        sb.append("\tprivate var state = \"INITIAL\"\n\tprivate val states:List[String] = List(\"INITIAL\",")
        sb.append(states.reduceLeft((a,b) => '"'+a+'"'+','+'"'+b+'"'))
        sb.append(")\n")
        sb.append("\tprivate val inclusive:Boolean = "+inclusive+'\n')
        //processing of input
        sb.append("\t/**file path of input to analyse*/\n")
        sb.append("\tprivate var _in:String = \"\"\n")
        sb.append("\t/**output stream from lexed input*/\n")
        sb.append("\tprivate var _out:List[Char] = Nil\n")
        sb.append("\t/**unprocessed input*/\n")
        sb.append("\tprivate var inputseq:List[Char] = Nil\n")
        sb.append("\tprivate var _c:Char = null\n")
        sb.append("\tprivate def readFile() = {\n")
        sb.append(Util.indentString(2)+"val bufferedSource = Source.fromFile(_in)\n")
        sb.append(Util.indentString(2)+"var lines:List[String] = List()\n")
        sb.append(Util.indentString(2)+"for (line <- bufferedSource.getLines) {\n")
        sb.append(Util.indentString(3)+"lines = lines:+line\n")
        sb.append(Util.indentString(3)+"logger.trace(line)\n")
        sb.append(Util.indentString(2)+"}\n")
        sb.append(Util.indentString(2)+"bufferedSource.close\n")
        //sb.append(Util.indentString(2)+"lines = lines.reverse\n")
        sb.append(Util.indentString(2)+"inputSeq = lines.flatten\n")
        sb.append(Util.indentString(2)+"_c = inputSeq.head\n")
        sb.append("\t}\n")
        //input(),output() and unput()
        sb.append("\t//IO methods\n")
        sb.append("\t/** gets the next character fron the input stream */\n")
        sb.append("\tprivate def input = _c\n")
        sb.append("\t/** writes a character to the output stream */\n")
        sb.append("\tprivate def output(c: Char) = {_out = _out:+c}\n")
        sb.append("\t/** writes a character to the input stream */\n")
        sb.append("\tprivate def unput(c: Char) = {inputSeq = inputSeq:+c}\n")
        //yylex
        sb.append("\tdef yylex() = {\n")
        //end yylex
        sb.append("\t}\n")
        if(withrules){processRules(in.head); in = in.tail}
        if(withroutines){processRoutines(in.head.head.asInstanceOf[CodeBlock]);in = in.tail}
        //end class
        sb.append('}')
        /** gets the definitions, options, code and states from the defs section */
        private def processDefs(d: List[GeneratorToken]) = {
            @tailrec
            def processDef(l: List[GeneratorToken]):Unit = l match {
                case Nil => {}
                case Definition(i,r)::xs => {defs = defs.updated(i(),r());processDef(xs)}
                case Declaration(s)::xs => {setOption(Util.asString(s));processDef(xs)}
                case LexingState(s,i)::xs => {states = s; inclusive = i; processDef(xs)}
                case CodeBlock(c)::xs => {sb.append(Util.asString(c)+'\n');processDef(xs)}
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
                    val rb = StringBuilder.newBuilder
                    val name = getId(s(),reg)
                    rb.append("\t//"+s()+", "+reg+'\n')
                    rb.append("\tprivate def " + name + "() = {\n"+Util.indentString(2))
                    rb.append(c())
                    rb.append("\n\t}\n")
                    rules = rules :+ rb.mkString
                    processRule(xs)
                }
                case Comment(c)::xs => processRule(xs)
                case x::xs => throw new LexerOutputError("Unexpected token in rules section: " + x)
            }
            processRule(r)
            sb.append("\n\t//### RULES ###\n")
            sb.append(linkRules)
            rules.map(s => sb.append("\n"+s))
        }
        /** adds the code from the routines section */
        private def processRoutines(c: CodeBlock) = {
            sb.append("\n\t//### USER SUBROUTINES ###\n\t")
            sb.append(c())
            sb.append("\n\t//### END ###\n")
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
        /** creates a switch statement that calls methods when regexes are activated */
        private def linkRules():String = {
            val b = StringBuilder.newBuilder
            b.append("\n\t/**selects the rule that matches a regex from yylex*/")
            b.append("\n\tprivate def doRule(r: String) = {\n")
            b.append(Util.indentString(2)+"r match {\n")
            for (((s,r),n)<- idMap) yield(b.append(Util.indentString(3)+"case \""+r+"\" if state == \""+s+"\" => " + n + "()\n"))
            b.append(Util.indentString(3)+"case y => {}\n"+Util.indentString(2)+"}\n\t}\n")
            b.mkString
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