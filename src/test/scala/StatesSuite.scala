package lexerGenerator

import org.junit._
import org.junit.Assert.assertEquals

class StatesSuite {
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
}