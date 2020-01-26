package lexerGenerator

import org.junit._
import org.junit.Assert.assertEquals

class LexerSuite {
    import lexerGenerator._

    @Ignore @Test def `LexSimpleInt`:Unit = {
        println("#########LexSimpleInt#########")
        val rules:String = "%%      \n      int k;\n[0-9]+   {\n        k = atoi(yytext);\n     if (k%7 == 0)\n             printf(\"%d\", k+3);\n      else\n          printf(\"%d\",k);\n         }"
        val lexer = lexerGenerator.Generator.ReadRules(rules)
    }

    //###### State tests ######
    @Test def `StateEquality`:Unit = {
        println("#########StateEquality#########")
        val s1 = new NFAState(0)
        val s2 = new NFAState(1)
        val s3 = new NFAState(0)
        assert(s1 == s3,"s1 == s3")
        assert(s1 != s2,"s1 != s2")
    }

    @Test def `StateSet`:Unit = {
        println("#########StateSet#########")
        val s1 = new NFAState(0)
        val s2 = new NFAState(1)
        //s3 should be equivalent in the set to s1
        val s3 = new NFAState(0)
        val nfaSet = Set(s1,s2)
        assert(nfaSet.contains(s1) && nfaSet.contains(s2),"s1s2")
        assert((nfaSet.contains(s3)),"s3")
    }

    //###### Char tests ######
    @Test def `OperatorPrecedence`:Unit = {
        println("#########OperatorPrecedence#########")
        assert(!expressions.precedence(expressions.backspace,'('))
    }

    //###### DFA Construction ######
    @Test def `DFAEmpty`:Unit = {
        println("#########DFAEmpty#########")
        val dfa = expressions.translateRegex("")
        assert(dfa.eval(""),"Empty")
    }

    @Test def `DFAConcat`:Unit = {
        println("#########DFAConcat#########")
        val seq1 = "a"
        val seq2 = "ba"
        val seq3 = "abb"
        val dfa = expressions.translateRegex("abb")
        assert(!dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
    }

    @Test def `DFAConcat2`:Unit = {
        println("#########DFAConcat2#########")
        val seq = "hello"
        val dfa = expressions.translateRegex("hello")
        assert(dfa.eval(seq),"Seq hello")
    }

    @Test def `DFAUnion`:Unit = {
        println("#########DFAUnion#########")
        val seq1 = "a"
        val seq2 = "b"
        val seq3 = "c"
        val dfa = expressions.translateRegex("a|b")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    @Test def `DFAStar`:Unit = {
        println("#########DFAStar#########")
        val seq1 = "a"
        val seq2 = "aa"
        val seq3 = "aaa"
        val seq4 = "aaaa"
        val seq5 = "aab"
        val dfa = expressions.translateRegex("a*")
        assert(dfa.eval(""),"empty")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
        assert(dfa.eval(seq4),"Seq4")
        assert(!dfa.eval(seq5),"Seq5")
    }

    @Ignore @Test def `DFAConcatStar`:Unit = {
        println("#########DFAConcatStar#########")
        val seq1 = "a"
        val seq2 = "ab"
        val seq3 = "abb"
        val seq4 = "abc"
        val dfa = expressions.translateRegex("ab*")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
        assert(!dfa.eval(seq4),"Seq4")
    }

    @Ignore @Test def `DFAStarUnion`:Unit = {
        println("#########DFAStarUnion#########")
        val seq1 = "aaa"
        val seq2 = "aa"
        val dfa = expressions.translateRegex("b|a*")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
    }

    @Ignore @Test def `DFADoubleStar`:Unit = {
        println("#########DFADoubleStar#########")
        val seq1 = "aaabbb"
        val seq2 = "aabb"
        val seq3 = "abc"
        val dfa = expressions.translateRegex("a*b*")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    @Ignore @Test def `DFABracket`:Unit = {
        println("#########DFABracket#########")
        val seq1 = "abcd"
        val seq2 = "(ab)(cd)"
        val dfa = expressions.translateRegex("(ab)(cd)")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
    }

    @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}