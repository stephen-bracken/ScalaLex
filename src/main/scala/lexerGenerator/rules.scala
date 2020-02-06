package lexerGenerator
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

abstract class rule(r: String, private val token:Token = new NoToken, private val action:Action = new Action){
    private val dfa:DFA = regexParser.translateRegex(r)
    def parse(s: String) = dfa.eval(s)
    def execute:Token = {action.execute; token}
}

class Action(s: String = "{}"){
    def execute = {
        val tb = currentMirror.mkToolBox()
        tb.eval(tb.parse(s))
    }
}