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
    @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}