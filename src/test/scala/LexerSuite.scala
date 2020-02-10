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

    @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}