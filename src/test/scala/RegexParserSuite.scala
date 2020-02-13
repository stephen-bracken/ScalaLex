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
        logger.info("#########DFAEmpty#########")
        val dfa = regexParser.translateRegex("")
        assert(dfa.eval(""),"Empty")
        assert(!dfa.eval("a"),"Not Empty")
    }

    it should "produce an equivalent DFA to abb" in {
        logger.info("#########DFAConcat#########")
        val seq1 = "a"
        val seq2 = "ba"
        val seq3 = "abb"
        val dfa = regexParser.translateRegex("abb")
        assert(!dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
        assert(dfa.eval(seq3),"Seq3")
    }

    it should "produce an equivalent DFA to hello" in {
        logger.info("#########DFAConcat2#########")
        val seq = "hello"
        val dfa = regexParser.translateRegex("hello")
        assert(dfa.eval(seq),"Seq hello")
    }

    it should "produce an equivalent DFA to a|b" in {
        logger.info("#########DFAUnion#########")
        val seq1 = "a"
        val seq2 = "b"
        val seq3 = "c"
        val dfa = regexParser.translateRegex("a|b")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    it should "produce a list of matches on an input string and its substrings" in {
        logger.info("#########DFAgetMatches#########")
        val in = "hellohellohellohellohellohellohellohellohello"
        val dfa = regexParser.translateRegex("hello")
        val r = dfa.getMatches(in)
        logger.info(r.toString)
    }

    it should "be able to tell whether there are any matches for an input string" in {
        logger.info("#########DFAanyMatches#########")
        val seq1 = "babaabaaaaaa befuogsebogbabababfiego"
        val seq2 = "gggggggggggggggggggggggg"
        val dfa = regexParser.translateRegex("a+")
        val r1 = dfa.getMatches(seq1)
        val r2 = dfa.getMatches(seq2)
        assert(!r1.isEmpty && dfa.anyMatches(seq1),"Seq1")
        assert(r2.isEmpty && !dfa.anyMatches(seq2),"Seq2")
    }

    it should "be able to get the longest match" in {
        logger.info("#########DFALongestMatch#########")
        val m1 = "baaaaaaa"
        val m2 = "caaaaaaa"
        assert(m1.length == m2.length)
        val seq1 = "baaaa " + m1 + " baa " + m2
        val seq2 = "ba ba baa"
        val dfa = regexParser.translateRegex("c|ba+")
        val r1 = dfa.longestMatch(seq1)
        val r2 = dfa.longestMatch(seq2)
        assert(r1._1 == m1.length && r1._2 == m1)
        assert(r2._1 == 3)
    }

    //###### Quote tests ######
    it should "produce an equivalent dfa to \"f*|\"" in {
        logger.info("#########DFAQuotes#########")
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
        logger.info("#########DFABadUnion#########")
        assertThrows[RegexError]{
            regexParser.translateRegex("a*|*")
        }
    }
}

class DFAStarSuite extends UnitSpec {
    //###### DFA Star tests ######
    "The Regex Parser" should "produce an equivalent DFA to a*" in {
        logger.info("#########DFAStar#########")
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
        logger.info("#########DFAPlus#########")
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
        logger.info("#########DFAConcatStar#########")
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
        logger.info("#########DFAStarUnion#########")
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
        logger.info("#########DFADoubleStar#########")
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
        logger.info("#########DFABracket#########")
        val seq1 = "abcd"
        val seq2 = "(ab)(cd)"
        val dfa = regexParser.translateRegex("(ab)(cd)")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
    }

    it should "produce an equivalent DFA to (abcd)*" in {
        logger.info("#########DFAStarBracket#########")
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
        logger.info("#########DFAUnionBracket#########")
        val seq1 = "abcd"
        val seq2 = "efgh"
        val seq3 = "abcdefgh"
        val dfa = regexParser.translateRegex("(abcd)|(efgh)")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    it should "produce an equivalent DFA to (abcd)|(efgh)*" in {
        logger.info("#########DFAUnionBracket#########")
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
        logger.info("#########DFACharSet#########")
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
        logger.info("#########DFARange#########")
        val r = 0 to 9
        val dfa = regexParser.translateRegex("[0-9]")
        for(i <- r) yield {assert(dfa.eval(i.toString),i.toString)}
    }

    it should "produce an equivalent DFA to [0\\-9]" in {
        logger.info("#########DFARangeBackslash#########")
        val seq1 = "-"
        val seq2 = "9"
        val seq3 = "5"
        val dfa = regexParser.translateRegex("[0\\-9]")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    it should "produce an equivalent DFA to [0-9]*" in {
        logger.info("#########DFARangeStar#########")
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
        logger.info("#########DFADoubleRange#########")
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
        logger.info("#########DFAMultiRange#########")
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
        logger.info("#########DFARangeOperators#########")
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
        logger.info("#########DFARangeDoubleBackslash#########")
        val seq1 = "\\"
        val dfa = regexParser.translateRegex("[\\\\]")
        assert(dfa.eval(seq1),"Seq1")
    }

    it should "produce an equivalent DFA to [^a-z]" in {
        logger.info("#########DFAInverseRange#########")
        val seq1 = "0"
        val seq2 = "a"
        val seq3 = "z"
        val dfa = regexParser.translateRegex("[^a-z]")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    it should "produce an equivalent DFA to [^]" in {
        logger.info("#########DFAInverseRange#########")
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
        logger.info("#########DFABackslashStar#########")
        val seq1 = "a*"
        val seq2 = "a"
        val seq3 = "aa"
        val dfa = regexParser.translateRegex("a\\*")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }

    "Regex a\\+\\*\\|" should "produce an equivalent DFA" in {
        logger.info("#########DFABackslashOperators#########")
        val seq1 = "a+*|"
        val dfa = regexParser.translateRegex("a\\+\\*\\|")
        assert(dfa.eval(seq1),"Seq1")
    }

    "Regex a\\(b\\)c" should "produce an equivalent DFA" in {
        logger.info("#########DFABackslashBrackets#########")
        val seq1 = "abc"
        val seq2 = "a(b)c"
        val dfa = regexParser.translateRegex("a\\(b\\)c")
        assert(!dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
    }

    "Regex \\\\" should "produce an equivalent DFA" in {
        logger.info("#########DFADoubleBackslash#########")
        val seq1 = "\\"
        val seq2 = "\\\\"
        val dfa = regexParser.translateRegex("\\\\")
        assert(dfa.eval(seq1),"Seq1")
        assert(!dfa.eval(seq2),"Seq2")
    }

    "Regex \\\\*" should "produce an equivalent DFA" in {
        logger.info("#########DFABackslashBrackets#########")
        val seq1 = "\\"
        val seq2 = "\\\\"
        val seq3 = "\\*"
        val dfa = regexParser.translateRegex("\\\\*")
        assert(dfa.eval(seq1),"Seq1")
        assert(dfa.eval(seq2),"Seq2")
        assert(!dfa.eval(seq3),"Seq3")
    }
}