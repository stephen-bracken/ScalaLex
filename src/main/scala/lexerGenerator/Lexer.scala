package lexerGenerator
abstract class Lexer(rules:List[Rule]){
    val words = for(r <- rules) yield {r.token}
    def yylex(input:String):List[Token]
}

class Token(val name:String) {
}

class NoToken extends Token("")