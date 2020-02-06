package lexerGenerator
abstract class Lexer(rules: List[LexRule]){
    def yylex(input:String):List[Token]
}

class Token(val name:String) {
}