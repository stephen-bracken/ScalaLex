package lexerGenerator

import org.junit._
import org.junit.Assert.assertEquals

class regexParserSuite {

    //###### Char tests ######
    @Test def `OperatorPrecedence`:Unit = {
        println("#########OperatorPrecedence#########")
        assert(!regexParser.precedence(regexParser.backspace,'('))
    }

    //###### DFA Construction ######
    @Test def `DFAEmpty`:Unit = {
        println("#########DFAEmpty#########")
        val dfa = regexParser.translateRegex("")
        assert(dfa.eval(""),"Empty")
        assert(!dfa.eval("a"),"Not Empty")
    }

    @Test def `DFAConcat`:Unit = {
        println("#########DFAConcat#########")
        val seq1 = "a"
        val seq2 = "ba"
        val seq3 = "abb"
        val dfa = regexParser.translateRegex("abb")
        assert(!dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
    }

    @Test def `DFAConcat2`:Unit = {
        println("#########DFAConcat2#########")
        val seq = "hello"
        val dfa = regexParser.translateRegex("hello")
        assert(dfa.eval(seq),"Seq hello")
    }

    @Test def `DFAUnion`:Unit = {
        println("#########DFAUnion#########")
        val seq1 = "a"
        val seq2 = "b"
        val seq3 = "c"
        val dfa = regexParser.translateRegex("a|b")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    //###### DFA Star tests ######
    @Test def `DFAStar`:Unit = {
        println("#########DFAStar#########")
        val seq1 = "a"
        val seq2 = "aa"
        val seq3 = "aaa"
        val seq4 = "aaaa"
        val seq5 = "aab"
        val dfa = regexParser.translateRegex("a*")
        assert(dfa.eval(""),"Empty")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
        assert(dfa.eval(seq4),"Seq4")
        assert(!dfa.eval(seq5),"Seq5")
    }

    @Test def `DFAPlus`:Unit = {
        println("#########DFAStar#########")
        val seq1 = "a"
        val seq2 = "aa"
        val seq3 = "aaa"
        val seq4 = "aaaa"
        val seq5 = "aab"
        val dfa = regexParser.translateRegex("a+")
        assert(!dfa.eval(""),"Empty")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
        assert(dfa.eval(seq4),"Seq4")
        assert(!dfa.eval(seq5),"Seq5")
    }

    @Test def `DFAConcatStar`:Unit = {
        println("#########DFAConcatStar#########")
        val seq1 = "a"
        val seq2 = "ab"
        val seq3 = "abb"
        val seq4 = "abc"
        val dfa = regexParser.translateRegex("ab*")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
        assert(!dfa.eval(seq4),"Seq4")
    }

    @Test def `DFAStarUnion`:Unit = {
        println("#########DFAStarUnion#########")
        val seq1 = "aaa"
        val seq2 = "aa"
        val seq3 = "b"
        val seq4 = "bb"
        val dfa = regexParser.translateRegex("b|a*")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
        assert(!dfa.eval(seq4),"Seq4")
    }

    @Test def `DFADoubleStar`:Unit = {
        println("#########DFADoubleStar#########")
        val seq1 = "aaabbb"
        val seq2 = "aabb"
        val seq3 = "abc"
        val dfa = regexParser.translateRegex("a*b*")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    //###### DFA Union tests ######
    @Test def `DFADoubleUnion`:Unit = {
        val seq1 = "a"
        val seq2 = "b"
        val seq3 = "c"
        val dfa = regexParser.translateRegex("a|b|c")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
    }

    //###### DFA Bracketing tests ######
    @Test def `DFABracket`:Unit = {
        println("#########DFABracket#########")
        val seq1 = "abcd"
        val seq2 = "(ab)(cd)"
        val dfa = regexParser.translateRegex("(ab)(cd)")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
    }

    @Test def `DFAStarBracket`:Unit = {
        println("#########DFAStarBracket#########")
        val seq1 = "abcd"
        val seq2 = "abcdabcd"
        val seq3 = "abcdefgh"
        val dfa = regexParser.translateRegex("(abcd)*")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
        assert(dfa.eval(""),"Empty")
    }

    @Test def `DFAUnionBracket`:Unit = {
        println("#########DFAUnionBracket#########")
        val seq1 = "abcd"
        val seq2 = "efgh"
        val seq3 = "abcdefgh"
        val dfa = regexParser.translateRegex("(abcd)|(efgh)")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    @Test def `DFAStarUnionBracket`:Unit = {
        println("#########DFAUnionBracket#########")
        val seq1 = "abcd"
        val seq2 = "efgh"
        val seq3 = "abcdefgh"
        val seq4 = "efghefgh"
        val dfa = regexParser.translateRegex("(abcd)|(efgh)*")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
        assert(dfa.eval(seq4),"Seq4")
    }

    @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}