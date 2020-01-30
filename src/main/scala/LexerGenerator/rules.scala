package lexerGenerator

abstract class rule(r: String, t:Token, a:Action){
    private val dfa:DFA = regexParser.translateRegex(r)
    def parse(s: String) = dfa.eval(s)
}

abstract class Action{}