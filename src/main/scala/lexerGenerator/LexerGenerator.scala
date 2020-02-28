package lexerGenerator

import com.typesafe.scalalogging.LazyLogging
import java.io.FileNotFoundException
import scala.io.Source
//import fastparse._,NoWhitespace._
import scala.annotation.tailrec
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.immutable.Nil


object Generator extends LazyLogging{
    type Section = (List[GeneratorToken],Boolean)
    private var defs:Section = (Nil,false)
    private var rules:Section = (Nil,false)
    private var routines:Section = (Nil,false)
    def main(args: Array[String]): Unit = {
        /** uses option flags to process command options */
        type OptionMap = Map[Symbol, Any]
        def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
            def isSwitch(s : String) = (s(0) == '-')
            list match {
                case Nil => map
                case "-i" :: value :: tail =>
                                    nextOption(map ++ Map('input -> value), tail)
                case "-o" :: value :: tail =>
                                    nextOption(map ++ Map('output -> value), tail)
                /*case string :: opt2 :: tail if isSwitch(opt2) => 
                                    nextOption(map ++ Map('infile -> string), list.tail)
                case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)*/
                case option :: tail => throw new GeneratorError("Invalid option: "+option)
                }
            }
        try{
            //read arguments from command
            val argList = args.toList
            val options = nextOption(Map(),argList)
            val currentDirectory = new java.io.File(".").getCanonicalPath
            val inputFile = options('input).asInstanceOf[String]
            logger.info("reading definitions from " + currentDirectory + '/' + inputFile)
            val outputFile = options('output).asInstanceOf[String] + ".scala"
            //read input file
            val bufferedSource = Source.fromFile(inputFile)
            var lines:List[String] = List()
            for (line <- bufferedSource.getLines) {
                lines = line + '\n' :: lines
            }
            bufferedSource.close
            lines = lines.reverse
            val compiledRules =  lex(lines.flatten)
            logger.info("Processed input: " + compiledRules)
            //logger.debug("final tokens: " + lines)
            //logger.info("writing output to " + outputFile)
            LexerFactory.withDefs(defs)
            LexerFactory.withRules(rules)
            LexerFactory.withRoutines(routines)
            val text = LexerFactory.makeLexer
            val file = new File(outputFile)
            val bw = new BufferedWriter(new FileWriter(file))
            bw.write(text)
            bw.close()
        }
        catch {
            case e:ArrayIndexOutOfBoundsException => usage
            case e:FileNotFoundException => throw new GeneratorError(e.getMessage())
        }
    }

    /** converts the input file into a lexed list of tokens */
    def lex(s: List[Char]) = {
        /** used with findSections, lexDefs and lexRules to specify composite tokens */
        var mode = 1
        /** keeps the previous mode during comments */
        var prevMode = 0
        /** contains the current set of characters to be consumed */
        var seq:List[Char] = Nil
        /** stores the identifier for Definitions */
        var ident:Identifier = null
        /** tracks when a block statement is being processed */
        var inBlock = false
        /** start of option declaration */
        val option = "%option".toList
        /** start of inclusive lexing state declaration */
        val inclusive = "%s".toList
        /** start of exclusive lexing state declaration */
        val exclusive = "%x".toList
        /** used to specify inclusive (true) or exclusive lexing states */
        var lexingstate = false
        /** used to store current list of lexing states */
        var states:List[String] = Nil
        /**
         * Lexes Definitions in the input file
         * modes:
         * 0 - no def
         * 1 - definition
         * 2 - option declaration
         * 3 - state transform
         * 4 - free code block
         * 5 - comment
         */
        def lexDefs(d: List[Char]):List[GeneratorToken] = {
            mode = 0
            seq = Nil
            inBlock = false
            var states:List[String] = Nil
            @tailrec
            def makeDef(d: List[Char],a: List[GeneratorToken]):List[GeneratorToken] = {
                d match {
                    case Nil if mode == 0 => a
                    case Nil if mode == 1 =>
                        val r = LexRegex(seq)
                        val d = Definition(ident,r)
                        logger.trace("processed regex definition: " + d)
                        seq = Nil
                        mode = 0
                        a :+ d
                    case Nil if mode == 2 => throw new GeneratorError("Unclosed option: " + a.last + seq)
                    case Nil if mode == 3 => 
                        states = states :+ Util.asString(seq)
                        seq = Nil
                        val s = new LexingState(states,lexingstate)
                        mode = 0
                        logger.trace("processed Lexing state: " + s)
                        a :+ s
                    case Nil if mode == 4 => throw new GeneratorError("Unclosed code block in defs: " + seq)
                    case Nil if mode == 5 => throw new GeneratorError("Unclosed comment in defs: " + seq)
                    //comments
                    case '/'::'*'::xs if mode == 0 =>
                        inBlock = true
                        logger.trace("processing def comment")
                        prevMode = mode
                        mode = 5
                        makeDef(xs,a)
                    case '*'::'/'::xs if mode == 5 =>
                        inBlock = false
                        logger.trace("returning to mode " + prevMode)
                        mode = prevMode
                        val c = Comment(seq)
                        logger.trace("processed comment: " + c)
                        seq = Nil
                        makeDef(xs,a:+c)
                    //code blocks
                    case ' '::' '::' '::' '::xs if mode == 0 =>
                        logger.trace("processing code block")
                        mode = 4
                        seq = Nil
                        makeDef(xs,a)
                    case '%'::'{'::xs if mode == 0 || mode == 4 =>
                        logger.trace("processing code block")
                        inBlock = true
                        mode = 4
                        seq = Nil
                        makeDef(xs,a)
                    case '}'::'%'::xs if mode == 4 =>
                        inBlock = false
                        mode = 0
                        val c = CodeBlock(seq)
                        logger.trace("added code block: " + c)
                        seq = Nil
                        makeDef(xs,a:+c)
                    //declaration end
                    case ','::xs if mode == 2 =>
                        seq =  Util.trimLeading(Util.asString(seq)).toList
                        val o = Declaration(seq)
                        logger.trace("processed option declaration: " + o)
                        mode = 0
                        seq = Nil
                        makeDef(xs,a :+ o)
                    case '\n'::xs if !inBlock =>
                        val r = mode match {
                            case 0 => a
                            case 1 =>
                                val l = LexRegex(seq)
                                logger.trace("processed regex: " + l)
                                val d = Definition(ident,l)
                                logger.trace("processed Definition: " + d)
                                a :+ d
                            case 2 => throw new GeneratorError("Unclosed option declaration: " + seq)
                            case 3 =>
                                states = states :+ seq.foldLeft("")((s,c) => s + c)
                                val l = LexingState(states,lexingstate)
                                logger.trace("processed lexing states: " + l)
                                a :+ l
                            case 4 => 
                                val c = CodeBlock(seq)
                                logger.trace("processed code block: " + c)
                                a :+ c
                            case 5 => throw new GeneratorError("Invalid comment in definitions section: " + seq)
                        }
                        if(mode != 0) {seq = Nil}
                        mode = 0
                        makeDef(xs,r)
                    //%option - declaration start
                    case x if seq == option && mode == 0 =>
                        mode = 2
                        logger.trace("processsing option declaration")
                        seq = Nil
                        makeDef(x,a)
                    //%s|%x - state definition
                    case x if (seq == inclusive || seq == exclusive) && mode == 0 =>
                        val s = x.span(c => c != '\n')
                        var n = Util.trimLeading(Util.asString(Util.trimWhitespace(s._1))).toList ++ s._2
                        logger.trace("processing lexing states")
                        mode = 3
                        lexingstate = seq == inclusive
                        seq = Nil
                        makeDef(n,a)
                    //separator
                    case ' '::xs if mode == 3 && !seq.filter(c => c != ' ').isEmpty =>
                        states = states :+ Util.asString(seq)
                        logger.trace("adding state " + seq)
                        seq = Nil
                        makeDef(xs,a)
                    //end of identifier
                    case ' '::xs if mode == 0 =>
                        ident = Identifier(seq)
                        logger.trace("processed identifier: " + ident)
                        val s = xs.span(c => c != '\n')
                        var n = Util.trimLeading(Util.asString(Util.trimWhitespace(s._1))).toList ++ s._2
                        seq = Nil
                        mode = 1
                        makeDef(n,a)
                    //normal character
                    case x::xs =>
                        //logger.trace(""+x)
                        seq = seq :+ x
                        makeDef(xs,a)
                }
            }
            makeDef(d,Nil)
        }
        /**
         * Lexes rule expressions in the input file
         * modes:
         * 0 - no mode
         * 1 - start condition
         * 2 - regex
         * 3 - code block
         * 4 - comment
         */
        def lexRules(r: List[Char]):List[GeneratorToken] ={ 
            mode = 0
            seq = Nil
            inBlock = false
            var start:StartCondition = StartCondition()
            var regex:LexRegex = null
            @tailrec
            def makeRule(r: List[Char],a: List[GeneratorToken]):List[GeneratorToken] = 
            r match {
                case Nil if mode == 0 => a
                case Nil if mode == 3 => 
                    val c = CodeBlock(seq)
                    logger.trace("processed code block: " + c)
                    seq = Nil
                    val r = LexingRule(start,regex,c)
                    mode = 0
                    start = StartCondition()
                    logger.trace("processed rule: " + r)
                    a :+ r
                //comment
                case '/'::'*'::xs if mode == 0 || mode == 2 =>
                    logger.trace("processing comment")
                    inBlock = true
                    prevMode = mode
                    mode = 4
                    makeRule(xs,a)
                case '*'::'/'::xs if mode == 4 =>
                    mode = prevMode
                    inBlock = false
                    val c = Comment(seq)
                    logger.trace("processed comment: " + c)
                    seq = Nil
                    makeRule(xs,a:+c)
                //start condition
                case '<'::xs if mode == 0 =>
                    logger.trace("processing start condition")
                    mode = 1
                    makeRule(xs,a)
                case '>'::xs if mode == 1 =>
                    mode = 2
                    start = StartCondition(seq)
                    logger.trace("processed start condition: " + start)
                    seq = Nil
                    makeRule(xs,a)
                //regex
                case ' '::' '::' '::' '::xs if mode == 2 || mode == 0 =>
                    regex = LexRegex(seq)
                    logger.trace("processed regex: " + regex)
                    seq = Nil
                    mode = 3
                    makeRule(xs,a)
                case '%'::'{'::xs if mode == 0 || mode == 2 || mode == 3 =>
                    logger.trace("processing code section")
                    inBlock = true
                    mode = 3
                    seq = Nil
                    makeRule(xs,a)
                case '}'::'%'::xs if mode == 3 =>
                    mode = 0
                    inBlock = false
                    val c = CodeBlock(seq)
                    logger.trace("processed code section: " + c)
                    val r = LexingRule(start,regex,c)
                    logger.trace("compiled rule: " + r)
                    seq = Nil
                    start = StartCondition()
                    makeRule(xs,a :+ r)
                    case '\n'::xs if !inBlock =>
                        val r = mode match {
                            case 0 => a
                            case 1 => throw new GeneratorError("unclosed start condition: " + seq)
                            case 2 => throw new GeneratorError("unclosed/unpaired regex expression: "+ seq)
                            case 3 => 
                                val c = CodeBlock(seq)
                                logger.trace("processed code block: " + c)
                                val r = LexingRule(start,regex,c)
                                logger.trace("compiled rule: " + r)
                                seq = Nil
                                start = StartCondition()
                                a :+ r
                            case 4 => throw new GeneratorError("Invalid comment in rules section: " + seq)
                        }
                        if(mode != 0) {seq = Nil}
                        mode = 0
                        makeRule(xs,r)
                case x::xs =>
                    seq = seq :+ x
                    makeRule(xs,a)
            }
            makeRule(r,Nil)
        }
        /**
         *   finds the ends of sections in the input file and calls appropriate lexers
         *   modes:
         *   0 - no section
         *   1 - defs
         *   2 - rules
         *   3 - routines
        */
        def findSections(s: List[Char]):List[GeneratorToken] = {
            var lines:List[Char] = Nil
            def makeDefs:Unit = {
                val d = lines
                logger.trace("reading defs from \n\"" + Util.asString(d) + '"')
                lines = Nil
                mode = 0
                defs = (lexDefs(d),true)
                logger.debug("lexed definitions: " + defs._1)
                mode = 2
                seq = Nil
            }
            def makeRules:Unit = {
                val r = lines
                lines = Nil
                logger.trace("reading rules from \n\"" + Util.asString(r) + '"')
                mode = 0
                rules = (lexRules(r),true)
                logger.debug("lexed rules: " + rules._1)
                mode = 3
                seq = Nil
            }
            def makeRoutines:Unit = {}
            @tailrec
            def find(s:List[Char],a:List[GeneratorToken]):List[GeneratorToken] = {
                s match {
                    case Nil => 
                        mode match{
                            case 1 => throw new GeneratorError("No delimiter found. the minimum input for an input file is:\n%%\n")
                            case 2 => makeRules; a ++ rules._1
                            case 3 => 
                                //gather code from final section
                                val r = lines
                                lines = Nil
                                mode = 0
                                if(!r.isEmpty){routines = (List(CodeBlock(r)),true)
                                logger.debug("lexed routines: " + routines._1)
                                //combine sections into result
                                a ++ routines._1}
                                else a
                            case x => throw new GeneratorError("Generator ended in unhandled mode: " + x)
                        }
                    case '%'::'%'::xs =>
                        val n:List[GeneratorToken] = mode match {
                            case 1 =>
                                makeDefs
                                defs._1 :+ Delimiter()
                            case 2 =>
                                makeRules
                                rules._1 :+ Delimiter()
                            case 0 =>
                                throw new GeneratorError("unexpected %% section")
                        }
                        find(xs,a ++ n)
                    case x::xs =>
                        lines = lines :+ x
                        find(xs,a)
                }
            }
            find(s,Nil)
        }
        findSections(s)
    }

    //def parser[_: P] = P( "hello" )
    /*def parseRules[_:P] = P(parseRule ~ parseNextRule)
    def parseNextRule[_:P]:P[Unit] = P(parseRule ~ parseNextRule | End)
    def parseRule[_:P] = P(parseRegex.! ~ parseBlock.!)
    def parseRegex[_:P] = P((anyButChar('\t').rep.! ~ "\t"))
    def startBlock[_: P] = P( "%{" ~ anyButChar('{').rep.! )
    def endBlock[_: P] = P( anyButSeq("}%").! ~ "}%" )
    def parseBlock[_: P] = P(codeBlock | (anyButChar(';').rep.! ~ ";"))
    def codeBlock[_:P] = P((startBlock ~ endBlock).!)
    /** matches any character except c */
    def anyButChar[_:P](c: Char) = P(CharPred(i => i != c))
    def anyButSeq[_:P](s: String):P[String] = s.toList match {
        case Nil => 
            throw new GeneratorError("anyButSeq with empty sequence")
        case x::Nil => 
            anyButChar(x).!
        case x::xs =>
            P(CharPred(c => c != x) ~ anyButSeq(xs.toString)).!
    }
    //def ws[_:P]:P[Unit] = P((" " | "\n").rep)
    */

    private def makeFile(l: List[GeneratorToken]) = {
        Util.asString(l.flatMap(t => t.toString()))
    }
    def usage = {
        logger.error("Usage: LexerGenerator -i [Definitions file] -o [Output filename]")
    }
}

/** token definitions used to lex the input file */
abstract class GeneratorToken(seq: List[Char]){
    override def toString(): String = {
        makeString()
    }
    /** creates a string representation of the collection in this token, with the leading string s and trailing string f */
    def makeString(s: String = "", f: String = "") = {
        val sb = StringBuilder.newBuilder
        sb.append(s)
        for (s <- seq) yield (sb.append(s))
        sb.append(f)
        sb.mkString
    }
}

/** represents a separator between sections in the input file */
case class Delimiter() extends GeneratorToken(Nil){
    override def toString():String = "%%"
}

case class Comment(s: List[Char]) extends GeneratorToken(s){
    override def toString(): String = {
        makeString("/*","*/")
    }
}

/** represents a Definition of an identifier with the associated regex */
case class Definition(i: Identifier, r: LexRegex) extends GeneratorToken(i.s ++ r.r){
    override def toString():String = i.toString() + ' ' + r.toString()
}

/** represents an option declaration in a lex input file */
case class Declaration(s: List[Char]) extends GeneratorToken(s){
    override def toString():String = makeString(s = "%option ",f=",")
}

/** represents one or more lexing states, e.g. INCOMMENT or INBRACKET, i.e. %x or %s
 *  followed by one or more state names
 *  @param s the list of lexing states
 *  @param i indicates whether the states are inclusive (%s) or exclusive (%x)
*/
case class LexingState(s: List[String],i: Boolean) extends GeneratorToken(s.flatMap(c => c :+ ',')){
    override def toString():String = {
        val sb = StringBuilder.newBuilder
        i match {
            case true => sb.append("%s ")
            case false => sb.append("%x ")
        }
        for(st <- s) yield (sb.append("{"+st+"} "))
        sb.mkString
    }
}

/** represents a rule of the form <startcondition> regex    %{code}% */
case class LexingRule(s: StartCondition, r: LexRegex, c: CodeBlock) extends GeneratorToken(s.s ++ r.r ++ c.c){
    override def toString():String = {
        s.toString() ++ r.toString() + '\t' + c.toString()
    }
    def getStart = s()
    def getRegex = r()
    def getCode = c()
}

/** the identifier component of a Definition */
case class Identifier(s: List[Char]) extends GeneratorToken(s){
    override def toString():String = {
        makeString("{","}")
    }
    def apply():String = Util.asString(s)
}

/** contains a regular expression. may also contain identifier names that need to be compiled */
case class LexRegex(r: List[Char]) extends GeneratorToken(r){
    def apply():String = Util.asString(r)
}

/** contains a start condition as declared in a LexingState */
case class StartCondition(s: List[Char] = "INITIAL".toList) extends GeneratorToken(s) {
    def apply():String = Util.asString(s)
    override def toString():String = makeString("<",">")
}

/** contains a scala expression */
case class CodeBlock(c: List[Char]) extends GeneratorToken(c) {
    def apply():String = Util.asString(c)
    override def toString():String = makeString("{","}")
}

class GeneratorError(msg: String) extends Error(msg) {

}