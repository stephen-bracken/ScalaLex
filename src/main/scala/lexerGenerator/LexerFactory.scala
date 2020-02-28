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
        /** gets the definitions, options, code and states from the defs section */
        private def processDefs(d: List[GeneratorToken]) = {

        }
        /** compiles the rules from the rules section into methods for yylex to call */
        private def processRules(r: List[GeneratorToken]) = {

        }
        /** adds the code from the routines section */
        private def processRoutines(c: CodeBlock) = {

        }
        /** looks up the regex value of a name from the defined names*/
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
        /** returns the output string */
        def apply() = {
            sb.mkString
        }
    }
    /** adds the tokens from the defs section to the output file */
    def withDefs(l: Section):Unit = {
        rawdefs = l._1
        withdefs = l._2
    }
    /** adds the tokens from the rules section to the output file */
    def withRules(l: Section):Unit = {
        rawrules = l._1
        withrules = l._2
    }
    /** adds the tokens from the routines section to the output file */
    def withRoutines(l: Section):Unit = {
        rawroutines = l._1
        withroutines = l._2
    }
    /** produces the output file text */
    def makeLexer = {
        var l:List[List[GeneratorToken]] = Nil
        if(withdefs){l = l :+ rawdefs}
        if(withrules){l = l :+ rawrules}
        if(withroutines){l = l :+ rawroutines}
        val o = new Output(l:_*)
        o()
    }
}