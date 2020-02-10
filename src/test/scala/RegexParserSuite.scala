package lexerGenerator

import org.junit._
import org.junit.Assert.assertEquals
import org.junit.rules.Timeout

abstract class RegexParserSuite {
    @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

class DFASuite extends RegexParserSuite {

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

    //###### Error tests ######
    @Test def `DFABadUnion`:Unit = {
        var b = false
        println("#########DFABadUnion#########")
        try{
            regexParser.translateRegex("a*|*")
        }
        catch{
            case e: Throwable => {
                val ex = new RegexError("a","b")
                assert(e.getClass==ex.getClass(),"getClass")
                b = true
            }
        }
        assert(b,"Catch")
    }
}

class DFAStarSuite extends RegexParserSuite {
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
        println("#########DFAPlus#########")
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
}

class DFAUnionSuite extends RegexParserSuite {
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


}

class DFABracketSuite extends RegexParserSuite {
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
}

class DFARangeSuite extends RegexParserSuite {
    //###### Char range tests ######
    @Test def `DFACharSet`:Unit = {
        println("#########DFACharSet#########")
        val seq1 = "a"
        val seq2 = "b"
        val seq3 = "c"
        val seq4 = "d"
        val dfa = regexParser.translateRegex("[abc]")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
        assert(!dfa.eval(seq4),"Seq4")
    }

    @Test def `DFARange`:Unit = {
        println("#########DFARange#########")
        val r = 0 to 9
        val dfa = regexParser.translateRegex("[0-9]")
        for(i <- r) yield {assert(dfa.eval(i.toString),i.toString)}
    }

    @Test def `DFARangeBackslash`:Unit = {
        println("#########DFARangeBackslash#########")
        val seq1 = "-"
        val seq2 = "9"
        val seq3 = "5"
        val dfa = regexParser.translateRegex("[0\\-9]")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    @Test def `DFARangeStar`:Unit = {
        println("#########DFARangeStar#########")
        val seq1 = "123"
        val seq2 = "456"
        val seq3 = "789"
        val dfa = regexParser.translateRegex("[0-9]*")
        assert(dfa.eval(""),"Empty")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
    }

    @Test def `DFADoubleRange`:Unit = {
        println("#########DFADoubleRange#########")
        val seq1 = "1a"
        val seq2 = "2b"
        val seq3 = "3c"
        val dfa = regexParser.translateRegex("[0-9][a-z]")
        assert(!dfa.eval(""),"Empty")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
    }

    @Test def `DFAMultiRange`:Unit = {
        println("#########DFAMultiRange#########")
        val seq1 = "a"
        val seq2 = "1"
        val seq3 = "1a"
        val dfa = regexParser.translateRegex("[0-9a-z]")
        assert(!dfa.eval(""),"Empty")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    @Test def `DFARangeOperators`:Unit = {
        println("#########DFARangeOperators#########")
        val seq1 = "+"
        val seq2 = "|"
        val seq3 = "*"
        val dfa = regexParser.translateRegex("[+|*]")
        assert(!dfa.eval(""),"Empty")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
    }

    @Test def `DFARangeDoubleBackslash`:Unit = {
        println("#########DFARangeDoubleBackslash#########")
        val seq1 = "\\"
        val dfa = regexParser.translateRegex("[\\\\]")
        assert(dfa.eval(seq1),"Seq1")
    }

    @Test def `DFAInverseRange`:Unit = {
        println("#########DFAInverseRange#########")
        val seq1 = "0"
        val seq2 = "a"
        val seq3 = "z"
        val dfa = regexParser.translateRegex("[^a-z]")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }
    
    override def individualTestTimeout: Timeout = new org.junit.rules.Timeout(600000)
}

class DFABackslashSuite extends RegexParserSuite {
        //###### Backslash tests ######
    @Test def `DFABackslashStar`:Unit = {
        println("#########DFABackslashStar#########")
        val seq1 = "a*"
        val seq2 = "a"
        val seq3 = "aa"
        val dfa = regexParser.translateRegex("a\\*")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    @Test def `DFABackslashOperators`:Unit = {
        println("#########DFABackslashOperators#########")
        val seq1 = "a+*|"
        val dfa = regexParser.translateRegex("a\\+\\*\\|")
        assert(dfa.eval(seq1),"Seq1")
    }

    @Test def `DFABackslashBrackets`:Unit = {
        println("#########DFABackslashBrackets#########")
        val seq1 = "abc"
        val seq2 = "a(b)c"
        val dfa = regexParser.translateRegex("a\\(b\\)c")
        assert(!dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
    }

    @Test def `DFADoubleBackslash`:Unit = {
        println("#########DFADoubleBackslash#########")
        val seq1 = "\\"
        val seq2 = "\\\\"
        val dfa = regexParser.translateRegex("\\\\")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
    }

    @Test def `DFADoubleBackslashOperator`:Unit = {
        println("#########DFABackslashBrackets#########")
        val seq1 = "\\"
        val seq2 = "\\\\"
        val seq3 = "\\*"
        val dfa = regexParser.translateRegex("\\\\*")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }
}