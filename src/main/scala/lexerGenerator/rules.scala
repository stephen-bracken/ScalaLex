package lexerGenerator
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import com.typesafe.scalalogging.LazyLogging

class LexRule(val startCondition: String = "", val regex: String = "",val action: String = "") extends LazyLogging{
    logger.trace("Creating LexRule with start condition:" + startCondition + ", regex:\"" + regex + "\", code action: %{" + action + "}%" )
    def this(s: List[String]) {
        this(s(0),s(1),s(2))
    }
    def this(s: List[Char], r: List[Char], c: List[Char]) {
        this(s.toString,r.toString,c.toString)
    }
    def this(s: StartCondition, r: LexRegex, c: CodeBlock){
        this(s(),r(),c())
    }
    def this(l: LexingRule){
        this(l.s(),l.r(),l.c())
    }
    private val dfa:DFA = regexParser.translateRegex(regex)
    def parse(s: String) = dfa.longestPrefixMatch(s)
    def execute = {
        logger.trace("Executing code action: \n" + action)
        val tb = currentMirror.mkToolBox()
        tb.eval(tb.parse('{' + action + '}'))
    }
    val result = execute
}