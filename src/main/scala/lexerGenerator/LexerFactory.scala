package scalaLex

import scala.annotation.tailrec
import scala.reflect.runtime._
import scala.reflect.runtime
import scala.tools.reflect.ToolBox
import java.io._

//TODO: Fix class path for DFA
object LexerFactory{
    type Section = (List[GeneratorToken],Boolean)
    private var fileName:String = ""
    private var rawdefs:List[GeneratorToken] = Nil
    private var rawrules:List[GeneratorToken] = Nil
    private var rawroutines:List[GeneratorToken] = Nil
    private var withdefs = false
    private var withrules = false
    private var withroutines = false
    private val file = new File("output/dfa")
    private val cm = universe.runtimeMirror(getClass().getClassLoader)
    private val tb = cm.mkToolBox()
    private val build = scala.reflect.runtime.universe.internal.reificationSupport
    private val oos = new ObjectOutputStream(new FileOutputStream(file))
    class Output(l: List[GeneratorToken]*){
        private var id = 0
        /** provides the function id for a regex to be exported to the final program */
        private var idMap:Map[String,String] = Map()
        private var regpair:List[(DFA,String,String)] = Nil
        private var in = l.toList
        private val sb: StringBuilder = StringBuilder.newBuilder
        /** (name -> regex) */
        private var defs:Map[String,String] = Map()
        private var rules:List[String] = Nil
        private var states:List[String] = Nil
        private var inclusive:Boolean = false
        private var options:Map[String,Boolean] = Map()
        sb.append("//######DEFINITIONS/OUTER EXPRESSIONS/IMPORTS######\n")
        //required imports
        sb.append("import scala.io.Source\n")
        sb.append("import scala.annotation.tailrec\n")
        sb.append("import java.io._\n")
        sb.append("import scalaLex.DFA\n")
        if(withdefs){processDefs(in.head);in = in.tail}
        //begin class
        sb.append("class Lex {\n")
        sb.append("\t//imports the state machines from the dfa file\n")
        sb.append("\tprivate val ois = new ObjectInputStream(new FileInputStream(\"dfa\"))\n")
        sb.append("\tprivate val regpair:List[(DFA,String,String)] = ois.readObject().asInstanceOf[List[(DFA,String,String)]] \n\tois.close()\n")
        sb.append("\treadFile()\n")
        //states
        sb.append("\t/**tracks the state of the lexer*/\n")
        sb.append("\tprivate var state = \"INITIAL\"\n")
        sb.append("\t/** list of valid lexing states */\n")
        sb.append("\tprivate val states:List[String] = List(\"INITIAL\",")
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
        sb.append("\t/**stores the next character to be processed*/\n")
        sb.append("\tprivate var _c:Char = '\\u0000'\n")
        sb.append("\tprivate var yytext:String = \"\"\n")
        sb.append("\tprivate var currMatch:String = \"\"\n")
        //readFile()
        sb.append("\t/**reads an input file into inputseq*/\n")
        sb.append("\tprivate def readFile() = {\n")
        sb.append(Util.indentString(2)+"val bufferedSource = Source.fromFile(_in)\n")
        sb.append(Util.indentString(2)+"var lines:List[String] = List()\n")
        sb.append(Util.indentString(2)+"println(\"reading lines from file:\")\n")
        sb.append(Util.indentString(2)+"for (line <- bufferedSource.getLines) {\n")
        sb.append(Util.indentString(3)+"lines = lines:+line\n")
        sb.append(Util.indentString(3)+"println(line)\n")
        sb.append(Util.indentString(2)+"}\n")
        sb.append(Util.indentString(2)+"bufferedSource.close\n")
        //sb.append(Util.indentString(2)+"lines = lines.reverse\n")
        sb.append(Util.indentString(2)+"inputseq = lines.flatten\n")
        sb.append(Util.indentString(2)+"_c = inputseq.head\n")
        //end readFile()
        sb.append("\t}\n")
        //input(),output() and unput()
        sb.append("\t//IO methods\n")
        sb.append("\t/** gets the next character fron the input stream */\n")
        sb.append("\tprivate def input = _c\n")
        sb.append("\t/** writes a character to the output stream */\n")
        sb.append("\tprivate def output(c: Char) = {_out = _out:+c}\n")
        sb.append("\t/** writes a character to the input stream */\n")
        sb.append("\tprivate def unput(c: Char) = {inputseq = c::inputseq}\n")
        sb.append("\t/** appends the current match to the matched text*/\n")
        sb.append("\tprivate def yymore() = {yytext += currMatch}\n")
        //yylex()
        sb.append("\tdef yylex() = {\n")
        sb.append(Util.indentString(2)+"@tailrec\n")
        sb.append(Util.indentString(2)+"def f(p:List[(DFA,String,String)],i:Int,a:String):Unit = {\n")
		sb.append(Util.indentString(3)+"p match {\n")
        sb.append(Util.indentString(4)+"case Nil =>\n")
        sb.append(Util.indentString(5)+"inputseq.splitAt(i)\n")
        sb.append(Util.indentString(5)+"currMatch = m._1.asInstanceOf[String]\n")
		sb.append(Util.indentString(5)+"inputseq = m._2\n")
		sb.append(Util.indentString(5)+"doRule(a)\n")
	    sb.append(Util.indentString(4)+"case (d,r,s) :: tl if s == state =>\n")
		sb.append(Util.indentString(5)+"val l = d.longestPrefixMatch(inputseq.asInstanceOf[String])\n")
	    sb.append(Util.indentString(5)+"if(l._1 > i) f(tl,l._1,a)\n")
        sb.append(Util.indentString(5)+"else f(tl,i,a)\n")
        sb.append(Util.indentString(4)+"case x::xs => f(xs,i,a)\n")
		sb.append(Util.indentString(3)+"}\n")
		sb.append(Util.indentString(2)+"}\n")
		sb.append(Util.indentString(2)+"f(regpair,0,\"\")\n")
        //end yylex()
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
                    val dfa = regexParser.translateRegex(reg)
                    val name = getId(s(),reg)(dfa)
                    rb.append(Util.indentString(2)+"//"+s()+", "+reg+'\n')
                    rb.append(Util.indentString(2)+"def " + name + "() = {\n"+Util.indentString(3))
                    rb.append(c())
                    rb.append("\n"+Util.indentString(2)+"}\n")
                    rules = rules :+ rb.mkString
                    processRule(xs)
                }
                case Comment(c)::xs => processRule(xs)
                case x::xs => throw new LexerOutputError("Unexpected token in rules section: " + x)
            }
            processRule(r)
            sb.append("\n\t//### RULES ###\n")
            sb.append("\n\t/**selects the rule that matches a regex from yylex*/")
            sb.append("\n\tprivate def doRule(r: String) = {\n")
            sb.append(Util.indentString(2)+"currMatch = r\n")
            sb.append(Util.indentString(2)+"yytext += r\n")
            rules.map(s => sb.append("\n"+s))
            sb.append(linkRules)
            sb.append("\t}\n")
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
        private def getId(s: String,r: String)(d: DFA):String = {
            val i = "rule" + id
            idMap = idMap.updated(r,i)
            regpair = (d,r,s) :: regpair
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
            b.append('\n'+Util.indentString(2)+"//Selector\n")
            b.append(Util.indentString(2)+"r match {\n")
            for ((r,n) <- idMap) yield(b.append(Util.indentString(3)+"case \""+r+"\" => " + n + "()\n"))
            b.append(Util.indentString(3)+"case y => {}\n"+Util.indentString(2)+"}\n")
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
        
        def writeMappings:Unit = {
            oos.writeObject(regpair)
        }
        
        /** returns the output string */
        def apply() = {
            oos.close()
            sb.mkString
        }
    }

    class SBTBuilder(l: List[(String,String)]){
        
    }

    def setFileName(s: String):Unit = {fileName = s}

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
        o.writeMappings
        val src = o()
        val file = new File("output/"+fileName)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(src)
        bw.close()
    }
}

class LexerOutputError(msg: String) extends Error(msg)