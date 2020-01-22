package lexerGenerator

import org.junit._
import org.junit.Assert.assertEquals

class LexerSuite {
    import lexerGenerator._

    @Ignore @Test def `LexSimpleInt`:Unit = {
        val rules:String = "%%      \n      int k;\n[0-9]+   {\n        k = atoi(yytext);\n     if (k%7 == 0)\n             printf(\"%d\", k+3);\n      else\n          printf(\"%d\",k);\n         }"
        val lexer = lexerGenerator.Generator.ReadRules(rules)
    }

    @Test def `StateEqualityExample`:Unit = {
        val s1 = new NFAState(0)
        val s2 = new NFAState(1)
        val s3 = new NFAState(0)
        assert(s1 == s3,"s1 == s3")
        assert(s1 != s2,"s1 != s2")
    }

    @Test def `StateSetExample`:Unit = {
        val s1 = new NFAState(0)
        val s2 = new NFAState(1)
        //s3 should be equivalent in the set to s1
        val s3 = new NFAState(0)
        val nfaSet = Set(s1,s2)
        assert(nfaSet.contains(s1) && nfaSet.contains(s2),"s1s2")
        assert((nfaSet.contains(s3)),"s3")
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