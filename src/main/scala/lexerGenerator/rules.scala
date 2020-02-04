package lexerGenerator
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

abstract class rule(r: String, t:Token, a:Action){
    private val dfa:DFA = regexParser.translateRegex(r)
    def parse(s: String) = dfa.eval(s)
}

class Action(s: String){
    val tb = currentMirror.mkToolBox()
    def execute = {
        tb.eval(tb.parse(s))
    }
}