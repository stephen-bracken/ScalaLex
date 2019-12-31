package lexerGenerator

import org.junit._
import org.junit.Assert.assertEquals

class LexerSuite {
    import lexerGenerator._

    @Ignore @Test def `LexSimpleInt`:Unit = {
        val rules:String = "%%      \n      int k;\n[0-9]+   {\n        k = atoi(yytext);\n     if (k%7 == 0)\n             printf(\"%d\", k+3);\n      else\n          printf(\"%d\",k);\n         }"
        val lexer = lexerGenerator.Generator.ReadRules(rules)
    }

    @Ignore @Test def `StateTransitionExample`:Unit = {
        val seq1 = "a"
        val seq2 = "ba"
        val seq3 = "abb"
        val dfa = expressions.translateRegex("ab*")
        assert(!dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
    }

    @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}