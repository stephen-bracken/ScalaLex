package lexerGenerator

import org.junit._
import org.junit.Assert.assertEquals

class ActionSuite {
    @Test def `noAction`:Unit = {
        val a = new Action
        a.execute
    }

    @Test def `ActionCompile`:Unit = {
        val a = new Action("println(\"hello\")")
        a.execute
    }

    @Test def `ActionResult`:Unit = {
        val a = new LexRule("",new Action("{3}"))
        assert(a.result == 3)
    }

    @Test def `ActionAddition`:Unit = {
        val a = new LexRule("",new Action("{3+4}"))
        assert(a.result == 7)
    }

    @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}