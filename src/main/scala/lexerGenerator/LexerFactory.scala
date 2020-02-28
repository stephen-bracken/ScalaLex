package lexerGenerator

object LexerFactory{
    type Section = (List[GeneratorToken],Boolean)
    private var o:Output = null
    private var rawdefs:List[GeneratorToken] = Nil
    private var rawrules:List[GeneratorToken] = Nil
    private var rawroutines:List[GeneratorToken] = Nil
    private var withdefs = false
    private var withrules = false
    private var withroutines = false
    class Output(l: List[GeneratorToken]*){
        private var in = l.toList
        private val sb: StringBuilder = StringBuilder.newBuilder
        private var defs:Map[String,String] = Map()
        if(withdefs){processDefs(in.head);in = in.tail}
        if(withrules){processRules(in.head); in = in.tail}
        if(withroutines){processRoutines(in.head.head.asInstanceOf[CodeBlock]);in = in.tail}
        private def processDefs(d: List[GeneratorToken]) = {

        }
        private def processRules(r: List[GeneratorToken]) = {

        }
        private def processRoutines(c: CodeBlock) = {

        }
        private def lookupDefs(s: String):String = {
            var n = s
            val r = (for{
                i <- 0 to s.length
                e <- i to s.length
            } yield {
                s.substring(i,e)}).filter(p => defs.keySet.contains(p))
                if(!r.isEmpty){for (i <- r) yield{n = n.replace(i,defs(i))}}
                n
            }
        def apply() = {
            sb.mkString
        }
    }
    private def reset = {
        withdefs = false
        withrules = false
        withroutines = false
        rawdefs = Nil
        rawrules = Nil
        rawroutines = null
    }
    def withDefs(l: Section):Unit = {
        rawdefs = l._1
        withdefs = l._2
    }
    def withRules(l: Section):Unit = {
        rawrules = l._1
        withrules = l._2
    }
    def withRoutines(l: Section):Unit = {
        rawroutines = l._1
        withroutines = l._2
    }
    def makeLexer = {
        var l:List[List[GeneratorToken]] = Nil
        if(withdefs){l = l :+ rawdefs}
        if(withrules){l = l :+ rawrules}
        if(withroutines){l = l :+ rawroutines}
        val o = new Output(l:_*)
        val s = o()
        reset
        s
    }
}