package lexerGenerator
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import com.typesafe.scalalogging.LazyLogging

class LexRule(regex: String = "", private val action:String = "") extends LazyLogging{
    private val dfa:DFA = regexParser.translateRegex(regex)
    def parse(s: String) = dfa.eval(s)
    def execute = {
        logger.trace("Executing code action: \n" + action)
        val tb = currentMirror.mkToolBox()
        tb.eval(tb.parse('{' + action + '}'))
    }
    val result = execute
}