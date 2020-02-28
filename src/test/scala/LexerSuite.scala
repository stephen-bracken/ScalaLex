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
        val in = "%%"
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
    /*
    /*"The Rule parser"*/ ignore should "Be able to parse the string ab+\t" in {
        println("########ParseRegex########")
        val str = "ab+\t"
        val res = "ab+"
        val parsedRegex = fastparse.parse(str,Generator.parseRegex(_))
        parsedRegex.isSuccess match {
            case true =>
                val Parsed.Success(r,i) = parsedRegex
                assert(r == res,"parser match")
            case false => 
                val Parsed.Failure(expected, index, extra) = parsedRegex
                fail(extra.trace().toString())
        }
        val r = parsedRegex.get.value
        val dfa = regexParser.translateRegex(r)
        assert(!dfa.eval(""),"Empty")
        assert(dfa.eval("ab"),"Seq1")
        assert(dfa.eval("abb"),"Seq2")
    }

    ignore should "be able to parse code blocks" in {
        val block = "%{3}%"
        val res = "3"
        val parsedBlock = fastparse.parse(block,Generator.codeBlock(_))
        parsedBlock.isSuccess match {
            case true =>
                val Parsed.Success(r,i) = parsedBlock
                assert(r == res,"parser match")
            case false => 
                val Parsed.Failure(expected, index, extra) = parsedBlock
                fail(extra.trace().toString())
        }
        val Parsed.Success(x,i) = parsedBlock
        //println(extra.trace())
        val a = new LexRule(action = x.asInstanceOf[String])
        assert(a.result == 3)
    }

    ignore should "be able to parse composite regexes and code blocks" in {
        println("#########FastParseRule#########")
        val rule1 = "a+\t%{3}%"
        val res1 = "a+"
        val res2 = "3"
        val parsedrule = fastparse.parse(rule1,Generator.parseRule(_))
        logger.info(parsedrule.toString())
        parsedrule.isSuccess match {
            case true =>
                val Parsed.Success((r,a),i) = parsedrule
                assert(r+a == res1+res2,"parser match")
            case false => 
                val Parsed.Failure(expected, index, extra) = parsedrule
                fail(extra.trace().toString())
        }
        val Parsed.Success((r,ac),i) = parsedrule
        logger.info("parsed regex: "+r)
        val reg = regexParser.translateRegex(r)
        assert(!reg.eval(""))
        assert(reg.eval("a"))
        assert(reg.eval("aa"))
        val a = new LexRule(regex = r,action = ac)
        assert(a.parse("a")._1 != 0)
        assert(a.result == 3)
    }*/

    //@Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}