package lexerGenerator

import org.scalatest.time._

//import org.junit._
//import org.junit.Assert.assertEquals
//import org.junit.rules.Timeout

/*abstract class RegexParserSuite {
    @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}*/

class DFASuite extends UnitSpec {

    //###### DFA Construction ######
    "The Regex Parser" should "produce an equivalent DFA to an empty regex" in {
        println("#########DFAEmpty#########")
        val dfa = regexParser.translateRegex("")
        assert(dfa.eval(""),"Empty")
        assert(!dfa.eval("a"),"Not Empty")
    }

    it should "produce an equivalent DFA to abb" in {
        println("#########DFAConcat#########")
        val seq1 = "a"
        val seq2 = "ba"
        val seq3 = "abb"
        val dfa = regexParser.translateRegex("abb")
        assert(!dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
    }

    it should "produce an equivalent DFA to hello" in {
        println("#########DFAConcat2#########")
        val seq = "hello"
        val dfa = regexParser.translateRegex("hello")
        assert(dfa.eval(seq),"Seq hello")
    }

    it should "produce an equivalent DFA to a|b" in {
        println("#########DFAUnion#########")
        val seq1 = "a"
        val seq2 = "b"
        val seq3 = "c"
        val dfa = regexParser.translateRegex("a|b")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    //###### Quote tests ######
    it should "produce an equivalent dfa to \"f*|\"" in {
        println("#########DFAQuotes#########")
        val seq1 = "f*|"
        val dfa = regexParser.translateRegex("\"f*|\"")
        assert(dfa.eval(seq1))
    }

    it should "be able to process backspace characters" in {
        val seq1:String = "" + backspace + backspace
        val dfa = regexParser.translateRegex(seq1)
        assert(dfa.eval(seq1))
    }

    //###### Error tests ######
    it should "not produce a DFA for a*|*" in {
        println("#########DFABadUnion#########")
        assertThrows[RegexError]{
            regexParser.translateRegex("a*|*")
        }
    }
}

class DFAStarSuite extends UnitSpec {
    //###### DFA Star tests ######
    "The Regex Parser" should "produce an equivalent DFA to a*" in {
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

    it should "produce an equivalent DFA to a+" in {
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

    it should "produce an equivalent DFA to ab*" in {
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

    it should "produce an equivalent DFA to b|a*" in {
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

    it should "produce an equivalent DFA to a*b*" in {
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

class DFAUnionSuite extends UnitSpec {
    //###### DFA Union tests ######
    "The Regex Parser" should "produce an equivalent DFA to a|b|c" in {
        val seq1 = "a"
        val seq2 = "b"
        val seq3 = "c"
        val dfa = regexParser.translateRegex("a|b|c")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
    }


}

class DFABracketSuite extends UnitSpec {
    //###### DFA Bracketing tests ######
    "The Regex Parser" should "produce an equivalent DFA to (ab)(cd)" in {
        println("#########DFABracket#########")
        val seq1 = "abcd"
        val seq2 = "(ab)(cd)"
        val dfa = regexParser.translateRegex("(ab)(cd)")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
    }

    it should "produce an equivalent DFA to (abcd)*" in {
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

    it should "produce an equivalent DFA to (abcd)|(efgh)" in {
        println("#########DFAUnionBracket#########")
        val seq1 = "abcd"
        val seq2 = "efgh"
        val seq3 = "abcdefgh"
        val dfa = regexParser.translateRegex("(abcd)|(efgh)")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    it should "produce an equivalent DFA to (abcd)|(efgh)*" in {
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

class DFARangeSuite extends UnitSpec {
    override def timeLimit: Span = Span(6000000,Millis)
    //###### Char range tests ######
    "The Regex Parser" should "produce an equivalent DFA to [abc]" in {
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

    it should "produce an equivalent DFA to [0-9]" in {
        println("#########DFARange#########")
        val r = 0 to 9
        val dfa = regexParser.translateRegex("[0-9]")
        for(i <- r) yield {assert(dfa.eval(i.toString),i.toString)}
    }

    it should "produce an equivalent DFA to [0\\-9]" in {
        println("#########DFARangeBackslash#########")
        val seq1 = "-"
        val seq2 = "9"
        val seq3 = "5"
        val dfa = regexParser.translateRegex("[0\\-9]")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    it should "produce an equivalent DFA to [0-9]*" in {
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

    it should "produce an equivalent DFA to [0-9][a-z]" in {
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

    it should "produce an equivalent DFA to [0-9a-z]" in {
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

    it should "produce an equivalent DFA to [+|*]" in {
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

    it should "produce an equivalent DFA to [\\\\]" in {
        println("#########DFARangeDoubleBackslash#########")
        val seq1 = "\\"
        val dfa = regexParser.translateRegex("[\\\\]")
        assert(dfa.eval(seq1),"Seq1")
    }

    it should "produce an equivalent DFA to [^a-z]" in {
        println("#########DFAInverseRange#########")
        val seq1 = "0"
        val seq2 = "a"
        val seq3 = "z"
        val dfa = regexParser.translateRegex("[^a-z]")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    it should "produce an equivalent DFA to [^]" in {
        println("#########DFAInverseRange#########")
        val seq1 = "a"
        val seq2 = "b"
        val seq3 = "c"
        val dfa = regexParser.translateRegex("[^]")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
    }
    
    //override def individualTestTimeout: Timeout = new org.junit.rules.Timeout(600000)
}

class DFABackslashSuite extends UnitSpec {
        //###### Backslash tests ######
    "Regex a\\*" should "produce an equivalent DFA" in {
        println("#########DFABackslashStar#########")
        val seq1 = "a*"
        val seq2 = "a"
        val seq3 = "aa"
        val dfa = regexParser.translateRegex("a\\*")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    "Regex a\\+\\*\\|" should "produce an equivalent DFA" in {
        println("#########DFABackslashOperators#########")
        val seq1 = "a+*|"
        val dfa = regexParser.translateRegex("a\\+\\*\\|")
        assert(dfa.eval(seq1),"Seq1")
    }

    "Regex a\\(b\\)c" should "produce an equivalent DFA" in {
        println("#########DFABackslashBrackets#########")
        val seq1 = "abc"
        val seq2 = "a(b)c"
        val dfa = regexParser.translateRegex("a\\(b\\)c")
        assert(!dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
    }

    "Regex \\\\" should "produce an equivalent DFA" in {
        println("#########DFADoubleBackslash#########")
        val seq1 = "\\"
        val seq2 = "\\\\"
        val dfa = regexParser.translateRegex("\\\\")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
    }

    "Regex \\\\*" should "produce an equivalent DFA" in {
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