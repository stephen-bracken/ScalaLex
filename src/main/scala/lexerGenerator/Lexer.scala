package lexerGenerator
abstract class Lexer(tokens: List[Token], actions: List[Action]){
    val words = tokens
    def yylex(input:String):List[Token]
}

trait Token {
    
}