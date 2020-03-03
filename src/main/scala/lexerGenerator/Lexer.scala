/*
    This file is an example/template for the output lexer.
*/
//######DEFINITIONS/OUTER EXPRESSIONS/IMPORTS######
import scala.io.Source
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
//...user imports/companion classes...


class Lex {
	/**tracks the state of the lexer*/
	private var state = "INITIAL"
	/** list of valid lexing states */
	private val states:List[String] = List("INITIAL")//list of lexing states
	private val inclusive:Boolean = false
	/**file path of input to analyse*/
	private var _in:String = ""
	/**output stream from lexed input*/
	private var _out:List[Char] = Nil
	/**unprocessed input*/
	private var inputseq:List[Char] = Nil
	/**stores the next character to be processed*/
	private var _c:Char = '\u0000'
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
	def yylex() = {
	}

	//### RULES ###

	/**selects the rule that matches a regex from yylex*/
	private def doRule(r: String) = {

        //...rules...

		//Selector
		r match {
            //case rule if state == ruleState => rulex()
			case y => {}
		}
	}

	//### USER SUBROUTINES ###
	{}
	//### END ###
}