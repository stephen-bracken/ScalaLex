package scalaLex

class RuleSuite extends UnitSpec {
    "an empty code block" should "execute with no actions" in {
        val a = new LexRule
        a.execute
    }

    "a code block" should "compile and run on match" in {
        val a = new LexRule(action = "println(\"hello\")")
        a.execute
    }

    it should "have an accessible result value" in {
        val a = new LexRule(action = "{3}")
        assert(a.result == 3)
    }

    it should "be able to perform operations at runtime" in {
        val a = new LexRule(action = "{3+4}")
        assert(a.result == 7)
    }
}