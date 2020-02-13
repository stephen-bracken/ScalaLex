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

    it should "have transitions to another state" in {
        println("#########StateTransition#########")
        val s0 = new NFAState(0)
        val s1 = new NFAState(1)
        s0.addTransition(false,s1,'a')
        assert(s0.transition('a').head == s1)
    }

    "An NFA State" should "have a reference within any DFAState that contain it" in {
        println("#########DFAState included#########")
        val s0 = new NFAState(0)
        val s1 = new DFAState(Set(s0),0)
        assert(s1.included(s0))
    }

    it should "know what states are accessible using no input symbol" in {
        println("#########NFAState epsilon#########")
        val s0 = new NFAState(0)
        val s1 = new NFAState(1)
        s0.epsilons_(s1)
        assert(s0.epsilons.contains(s1))
    }

    "A dead end state" should "have no state transitions that do not result in itself" in {
        println("#########StateDeadEnd#########")
        val s0 = new DFAState(Set(),0)
        val s1 = new DFAState(Set(),1)
        val s2 = new DFAState(Set(),2)
        val s3 = new DFAState(Set(),3)
        s0.addTransition(false,s1,'a')
        s1.accepting = true
        s2.addTransition(false,s2,'a')
        assert(!s0.deadEnd,"S0")
        assert(!s1.deadEnd,"S1")
        assert(s2.deadEnd,"S2")
        assert(s3.deadEnd,"S3")

    }
}