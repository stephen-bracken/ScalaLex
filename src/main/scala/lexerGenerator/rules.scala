package lexerGenerator
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import com.typesafe.scalalogging.LazyLogging

class LexRule(val startCondition: String = "", val regex: String = "",val action: String = "") extends LazyLogging{
    def this(s: List[String]) {
        this(s(0),s(1),s(2))
    }
    private val dfa:DFA = regexParser.translateRegex(regex)
    def parse(s: String) = dfa(s)
    def execute = {
        logger.trace("Executing code action: \n" + action)
        val tb = currentMirror.mkToolBox()
        tb.eval(tb.parse('{' + action + '}'))
    }
    val result = execute
}