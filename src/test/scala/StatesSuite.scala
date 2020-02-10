package lexerGenerator

//import org.junit._
//import org.junit.Assert.assertEquals

class StatesSuite extends UnitSpec {
    //###### State tests ######
    "A State" should "Be comparable by id" in  {
        println("#########StateEquality#########")
        val s1 = new NFAState(0)
        val s2 = new NFAState(1)
        val s3 = new NFAState(0)
        assert(s1 == s3,"s1 == s3")
        assert(s1 != s2,"s1 != s2")
    }

    it should "be comparable in a set" in {
        println("#########StateSet#########")
        val s1 = new NFAState(0)
        val s2 = new NFAState(1)
        //s3 should be equivalent in the set to s1
        val s3 = new NFAState(0)
        val nfaSet = Set(s1,s2)
        assert(nfaSet.contains(s1) && nfaSet.contains(s2),"s1s2")
        assert((nfaSet.contains(s3)),"s3")
    }

    "A dead end state" should "have no state transitions that do not result in itself" in {
        println("#########StateDeadEnd#########")
        val s0 = new DFAState(Set(),0)
        val s1 = new DFAState(Set(),1)
        val s2 = new DFAState(Set(),2)
        val s3 = new DFAState(Set(),3)
        s0.addTransition('a',s1)
        s1.accepting = true
        s2.addTransition('a',s2)
        assert(!s0.deadEnd,"S0")
        assert(!s1.deadEnd,"S1")
        assert(s2.deadEnd,"S2")
        assert(s3.deadEnd,"S3")

    }
}