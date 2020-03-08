package lexerGenerator

//import org.junit._
//import org.junit.Assert.assertEquals

//import fastparse._

class LexerSuite extends UnitSpec {

    "The Generator" should "exist" in {
        logger.info("#########Generator exists#########")
        Generator.usage
        succeed
    }

    it should "execute fully" in {
        logger.info("#########Generator main method#########")
        val args = List("-i","input.txt","-o","output")
        Generator.main(args.toArray)
        succeed
    }

    it should "Be able to process the input rules" in {
        println("#########LexSimpleInt#########")
        val rules:String = "%%      \n      int k;\n[0-9]+   {\n        k = atoi(yytext);\n     if (k%7 == 0)\n             printf(\"%d\", k+3);\n      else\n          printf(\"%d\",k);\n         }"
        //val lexer = lexerGenerator.Generator.ReadRules(List(rules))
    }

    it should "Be able to process an empty program" in {
        logger.info("#########LexEmptyDefs#########")
        val in = "\n%%"
        assert(Generator.lex(in.toList) == List(Delimiter()))
    }

    it should "Be able to process comments" in {
        logger.info("#########LexTokens#########")
        val in = "/* I am a comment */\n%%/* I am a comment */"
        val r = Generator.lex(in.toList)
        assert(r.filter(p => p.isInstanceOf[Comment]).length == 2)
    }

    it should "Be able to process each Lexical token" in {
        logger.info("#########LexTokens#########")
        val in = "/* I am a comment */\nnumber [0-9]+\n%option case-insensitive,\n%x INSTRING INCOMMENT\n%%\n/* I am in the rules section */\nhello\tprintln(\"hello\");\nworld\tprintln(\"world\");\n%%\nprintln(\"I am in the code section\");\n"
        logger.info(in)
        val r = Generator.lex(in.toList)
        logger.info(r.toString)
    }
}