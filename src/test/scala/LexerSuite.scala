package lexerGenerator

//import org.junit._
//import org.junit.Assert.assertEquals


class LexerSuite extends UnitSpec {

    ignore /*"The Generator"*/ should "execute fully" in {
        val arg1 = "input.txt"
        val arg2 = "output"
        Generator.main(Array(arg1,arg2))
    }

    ignore /*it*/ should "Be able to process the input rules" in {
        println("#########LexSimpleInt#########")
        val rules:String = "%%      \n      int k;\n[0-9]+   {\n        k = atoi(yytext);\n     if (k%7 == 0)\n             printf(\"%d\", k+3);\n      else\n          printf(\"%d\",k);\n         }"
        //val lexer = lexerGenerator.Generator.ReadRules(List(rules))
    }

    "The Rule parser" should "Be able to parse the string ab+\t" in {
        val str = "ab+\t"
        /*val parsedRegex = fastparse.parse(str,Generator.parseRegex(_))
        val Parsed.Failure(expected, index, extra) = parsedRegex
        println(extra.trace())*/
    }

    it should "be able to parse code blocks" in {
        val block = "{3}"
        /*val parsedBlock = fastparse.parse(block,Generator.parseBlock(_))
        val Parsed.Failure(expected, index, extra) = parsedBlock
        println(extra.trace())
        val a = new LexRule(action = parsedBlock.get.value)
        assert(a.result == 3)*/
    }

    it should "be able to parse composite regexes and code blocks" in {
        println("#########FastParseRule#########")
        val rule1 = "a+\t{3}"
        /*val parsedrule = fastparse.parse(rule1,Generator.parseRule(_))
        println(parsedrule)
        val Parsed.Failure(expected, index, extra) = parsedrule
        println(extra.trace())
        /*val reg = regexParser.translateRegex(parsedrule.get.value)
        assert(!reg.eval(""))
        assert(reg.eval("a"))
        assert(reg.eval("aa"))
        val a = new Action(parsedrule.get.value._2)
        a.execute*/
        println(parsedrule.get.value)*/
    }

    //@Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}