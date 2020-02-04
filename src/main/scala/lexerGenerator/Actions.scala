package lexerGenerator

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

class Action(s: String){
    def execute = {
        val tb = currentMirror.mkToolBox()
        tb.eval(tb.parse(s))
    }
}