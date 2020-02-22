package lexerGenerator

import com.typesafe.scalalogging.LazyLogging
import java.io.FileNotFoundException
import scala.io.Source
//import fastparse._,NoWhitespace._
import scala.annotation.tailrec


object Generator extends LazyLogging{
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
            val outputFile = options('output).asInstanceOf[String]
            //read input file
            val bufferedSource = Source.fromFile(inputFile)
            var lines:List[String] = List()
            for (line <- bufferedSource.getLines) {
                lines = line :: lines
                logger.trace(line)
            }
            bufferedSource.close
            lines = lines.reverse
            /*val split = lines.span(x => !(x.startsWith("%%")))
            val defs = split._1
            val rest = split._2
            val rest2 = rest.span(x => !(x.startsWith("%%")))
            val rules = rest2._1
            val routines = rest2._2*/
            val compiledRules =  lex(lines.reduceLeft((x,y) => x ++ y))
            logger.trace("final text: " + lines)
            logger.info("writing output to " + outputFile)
        }
        catch {
            case e:ArrayIndexOutOfBoundsException => usage
            case e:FileNotFoundException => throw new GeneratorError(e.getMessage())
        }
    }

    def trimWhitespace(s: List[Char],a: List[Char]):List[Char] = s match {
        case Nil => a
        case ' '::Nil => a
        case ' '::' '::xs =>
            trimWhitespace(' ' +: xs,a)
        case x::xs =>
            trimWhitespace(xs,a :+ x)
    }

    def lex(s: String) = {
        /** used with seq, code, states etc. to specify composite tokens */
        var mode = 1
        var prevMode = 0
        var section:List[Char] = Nil
        var defs:List[GeneratorToken] = Nil
        var rules:List[GeneratorToken] = Nil
        var routines:List[GeneratorToken] = Nil
        var seq:List[Char] = Nil
        var code:List[Char] = Nil
        var expr:List[GeneratorToken] = Nil
        var ident:Identifier = null
        var start:StartCondition = null
        /* start of option declaration */
        val option = "%option".toList
        /* start of inclusive lexing state declaration */
        val inclusive = "%s".toList
        /* start of exclusive lexing state declaration */
        val exclusive = "%x".toList
        var lexingstate = false
        var states:List[String] = Nil
        /*
        modes:
            0 - no def
            1 - declaration
            2 - option
            3 - state transform
            4 - free code block
        */
        @tailrec
        def lexDefs(d: List[Char],a: List[GeneratorToken]):List[GeneratorToken] = d match {
            case Nil => mode = 2; a
            //identifier
            case '_'::xs if mode == 0 =>
                ident = Identifier(seq)
                val s = xs.span(c => c != '\n')
                var n = trimWhitespace(s._1,Nil).toString.stripLeading.toList ++ s._2
                seq = Nil
                mode = 1
                lexDefs(n,a)
            case '%'::'{'::xs if mode == 0 =>
                mode = 4
                seq = Nil
                lexDefs(xs,a)
            case '}'::'%'::xs if mode == 4 =>
                mode = 0
                val c = CodeBlock(seq)
                seq = Nil
                lexDefs(xs,a:+c)
            case '\n'::xs if mode == 1 =>
                val r = LexRegex(seq)
                val d = Definition(ident,r)
                seq = Nil
                mode = 0
                lexDefs(xs,a :+ d)
            //%option
            case ','::xs if mode == 3 =>
                val o = Declaration(seq)
                seq = Nil
                lexDefs(xs,a :+ o)
            case x if seq == option && mode == 0 =>
                mode = 3
                seq = Nil
                lexDefs(x,a)
            //lexing states
            case '\n'::xs if mode == 4 =>
                {
                states = states :+ seq.toString
                val s = LexingState(states,lexingstate)
                seq = Nil
                mode = 0
                lexDefs(xs,a :+ s)
                }
            //separator
            case ' '::xs if mode == 4 =>
                states = states :+ seq.toString
                seq = Nil
                lexDefs(xs,a)
            //%s and %x
            case x if (seq == inclusive || seq == exclusive) && mode == 0 =>
                {
                val s = x.span(c => c != '\n')
                val n = trimWhitespace(s._1,Nil) ++ s._2
                mode = 4
                lexingstate = seq == inclusive
                seq = Nil
                lexDefs(n,a)
                }
            //normal character
            case x::xs =>
                seq = seq :+ x
                lexDefs(xs,a)
        }
        /*
        modes:
        0 - no mode
        1 - start condition
        2 - regex
        3 - code block
        4 - comment
        */
        @tailrec
        def lexRules(r: List[Char],a: List[GeneratorToken]):List[GeneratorToken] = r match {
            case Nil => mode = 3; a
            case '/'::'*'::xs if mode == 0 || mode == 2 =>
                prevMode = mode
                mode = 4
                lexRules(xs,a)
            case '*'::'/'::xs if mode == 4 =>
                mode = prevMode
                val c = Comment(seq)
                lexRules(xs,a:+c)
            case '<'::xs if mode == 0 =>
                mode = 1
                lexRules(xs,a)
            case '>'::xs if mode == 1 =>
                mode = 2
                val s = StartCondition(seq)
                seq = Nil
                lexRules(xs,a :+ s)
            case '\t'::xs if mode == 2 =>
                val r = LexRegex(seq)
                seq = Nil
                mode = 0
                lexRules(xs,a :+ r)
            case '%'::'{'::xs if mode == 0 =>
                mode = 3
                seq = Nil
                lexRules(xs,a)
            case '}'::'%'::xs if mode == 3 =>
                mode = 0
                val c = CodeBlock(seq)
                seq = Nil
                lexRules(xs,a :+ c)
            case '\n'::xs if mode == 3 =>
                mode = 0
                val c = CodeBlock(seq)
                seq = Nil
                lexRules(xs,a)
            case x::xs =>
                seq = seq :+ x
                lexRules(xs,a)
        }
        /*
        modes:
            0 - no section
            1 - defs
            2 - rules
            3 - routines
        */
        @tailrec
        def findSections(s: List[Char]):List[GeneratorToken] = {
        s.toList match {
            case Nil => 
                if(mode == 1) throw new GeneratorError("No delimiter found. the minimum input for an input file is:\n%%\n")
                //gather code from final section
                val r = seq
                seq = Nil
                mode = 0
                routines = List(CodeBlock(r))
                //combine sections into result
                defs ++ rules ++ routines
            case '%'::'%'::xs =>
                val n = mode match {
                    case 1 =>
                        val d = seq
                        seq = Nil
                        mode = 0
                        defs = lexDefs(d,Nil) :+ Delimiter()
                    case 2 =>
                        val r = seq
                        seq = Nil
                        mode = 0
                        rules = lexRules(r,Nil) :+ Delimiter()
                    case 0 =>
                        throw new GeneratorError("unexpected %% section")
                }
                findSections(xs)
            case x::xs =>
                seq = seq :+ x
                findSections(xs)
            }
        }
        findSections(s.toList)
    }

    /*def readRules(rules: List[GeneratorToken]):List[LexRule] = {
        /*var inCode = false
        @tailrec
        def getCode(s: List[Char],a: List[Char]):List[Char] = {
            s match {
                case Nil => a.reverse
                case '}'::'%'::xs if inCode =>
                    a.reverse
                case '%'::'{'::xs if !inCode =>
                    inCode = true
                    getCode(xs,a)
                case x::xs if !inCode =>
                    getCode(xs,a)
            }
        }
        rules match {
        case Nil => 
            Nil
        case x :: xs =>
            var p = x
            var args:List[String] = Nil
            if(x.takeWhile(c => c != '\t').contains(List('<','>'))){
                val span = x.span(c => c != '>')
                val startCond = span._1.tail
                args = args :+ startCond
                p = span._2.tail
            }
            else {args = args :+ "_"}
            val r = p.span(c => c != '\t')
            args = args :+ r._1.stripTrailing :+ getCode(r._2.toList,Nil).toString
            val rule = new LexRule(args) 
        }*/
        def makeRule(r: List[GeneratorToken],a: List[LexRule]):List[LexRule] = {
            r match {
                case Nil => a
                case StartCondition(s)::LexRegex(r)::CodeBlock(c)::xs =>
                    makeRule(xs,a :+ new LexRule(s,r,c))
                case LexRegex(r)::CodeBlock(c)::xs =>
                    makeRule(xs,a :+ new LexRule(Nil,r,c))
                case LexRegex(r)::xs =>
                    makeRule(xs, a :+ new LexRule(Nil,r,Nil))
                case CodeBlock(c)::xs => 
                    throw new GeneratorError("Unbound codeblock in rules: " + c)
                case StartCondition(s)::xs =>
                    throw new GeneratorError("Unbound start condition in rules: " + s)
                case x::xs =>
                    throw new GeneratorError("Unknown input: " + x)
            }
        }
        makeRule(rules,Nil)
    }*/
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
    def usage = {
        logger.error("Usage: LexerGenerator -i [Definitions file] -o [Output filename]")
    }
}

/** token definitions used to lex the input file */
abstract class GeneratorToken(seq: List[Char]){
    override def toString(): String = {
        makeString()
    }
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
    override def toString():String = makeString(s = "%option")
}

/** represents one or more lexing states, e.g. INCOMMENT or INBRACKET, i.e. %x or %s
 *  followed by one or more state names
 *  @param s the list of lexing states
 *  @param i indicates whether the states are inclusive (%s) or exclusive (%x)
*/
case class LexingState(s: List[String],i: Boolean) extends GeneratorToken(s.flatten){
    override def toString():String = {
        val s = i match {
            case true => "%s"
            case false => "%x"
        }
        makeString(s = s)
    }
}

/** represents a rule of the form <startcondition> regex    %{code}% */
case class LexingRule(s: StartCondition = StartCondition(Nil), r: LexRegex, c: CodeBlock) extends GeneratorToken(s.s ++ r.r ++ c.c){
    override def toString():String = {
        s.toString() ++ r.toString() + '\t' + c.toString()
    }
    def getStart = s.toString
    def getRegex = r.toString
    def getCode = c.toString
}

/** the identifier component of a Definition */
case class Identifier(s: List[Char]) extends GeneratorToken(s){
    override def toString():String = {
        makeString("{","}")
    }
}

/** contains a regular expression. may also contain identifier names that need to be compiled */
case class LexRegex(r: List[Char]) extends GeneratorToken(r)

/** contains a start condition as declared in a LexingState */
case class StartCondition(s: List[Char]) extends GeneratorToken(s) {
    override def toString():String = makeString("<",">")
}

/** contains a scala expression */
case class CodeBlock(c: List[Char]) extends GeneratorToken(c) {
    override def toString():String = makeString("{","}")
}

class GeneratorError(msg: String) extends Error(msg) {

}