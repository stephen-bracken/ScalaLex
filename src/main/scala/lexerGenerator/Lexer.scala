/*
    This file is an example/template for the output lexer.
*/
//######DEFINITIONS/OUTER EXPRESSIONS/IMPORTS######
import scala.io.Source
import java.io._
import scalaLex.DFA
import com.typesafe.scalalogging.LazyLogging
import scala.annotation.tailrec

    object Tester{
        println("hello")
    }

class Lex {
	//imports the state machines from the dfa file
	private val ois = new ObjectInputStream(new FileInputStream("dfa"))
	private val regpair:List[(DFA,String,String)] = ois.readObject().asInstanceOf[List[(DFA,String,String)]] 
	ois.close()
	readFile()
	/**tracks the state of the lexer*/
	private var state = "INITIAL"
	/** list of valid lexing states */
	private val states:List[String] = List("INITIAL","INSTRING","INCOMMENT")
	private val inclusive:Boolean = false
	/**file path of input to analyse*/
	private var _in:String = ""
	/**output stream from lexed input*/
	private var _out:List[Char] = Nil
	/**unprocessed input*/
	private var inputseq:List[Char] = Nil
	/**stores the next character to be processed*/
	private var _c:Char = '\u0000'
	private var yytext:String = ""
	private var currMatch:String = ""
	/**reads an input file into inputseq*/
	private def readFile() = {
		val bufferedSource = Source.fromFile(_in)
		var lines:List[String] = List()
		println("reading lines from file:")
		for (line <- bufferedSource.getLines) {
			lines = lines:+line
			println(line)
		}
		bufferedSource.close
		inputseq = lines.flatten
		_c = inputseq.head
	}
	//IO methods
	/** gets the next character fron the input stream */
	private def input = _c
	/** writes a character to the output stream */
	private def output(c: Char) = {_out = _out:+c}
	/** writes a character to the input stream */
	private def unput(c: Char) = {inputseq = c::inputseq}
	/** appends the current match to the matched text*/
	private def yymore() = {yytext += currMatch}
	def yylex() = {
		@tailrec
		def f(p:List[(DFA,String,String)],i:Int,a:String):Unit = {
			p match {
				case Nil => 
					val m = inputseq.splitAt(i)
					currMatch = m._1.asInstanceOf[String]
					inputseq = m._2
					doRule(a)
				case (d,r,s) :: tl if s == state =>
					val l = d.longestPrefixMatch(inputseq.asInstanceOf[String])
					if(l._1 > i) f(tl,l._1,a)
					else f(tl,i,a)
			}
		}
		f(regpair,0,"")
	}

	//### RULES ###

	/**selects the rule that matches a regex from yylex*/
	private def doRule(r: String) = {
		currMatch = r
		yytext += r

		//INSTRING, a*
		def rule0() = {
			println("AAAAAAAAAAAAAAAA");
		}

		//INITIAL, hello
		def rule1() = {
			println("hello");
		}

		//INITIAL, world
		def rule2() = {
			println("world");
		}

		//INITIAL, rule[0-9]+
		def rule3() = {
			println("hi");
		}

		//INITIAL, c2
		def rule4() = {
			
    println("code block 2")

		}

		//INITIAL, add
		def rule5() = {
			2 + 4
		}

		//Selector
		r match {
			case "rule[0-9]+" => rule3()
			case "c2" => rule4()
			case "a*" => rule0()
			case "add" => rule5()
			case "hello" => rule1()
			case "world" => rule2()
			case y => {}
		}
	}

	//### USER SUBROUTINES ###
	
println("I am in the code section");
def main(args: Array[String]): Unit = {
    while(inputseq.nonEmpty){
		yylex()
	}
}
	//### END ###
}