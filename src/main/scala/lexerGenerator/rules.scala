package lexerGenerator
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import com.typesafe.scalalogging.LazyLogging

class LexRule(r: String, private val action:Action = new Action){
    private val dfa:DFA = regexParser.translateRegex(r)
    def parse(s: String) = dfa.eval(s)
    def execute = action.execute
    val result = execute
}

class Action(s: String = "") extends LazyLogging {
    def execute = {
        logger.trace("Executing code action: \n" + s)
        val tb = currentMirror.mkToolBox()
        tb.eval(tb.parse('{' + s + '}'))
    }
}