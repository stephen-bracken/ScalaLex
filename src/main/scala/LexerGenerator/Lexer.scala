package lexerGenerator
abstract class Lexer(tokens: Token){
    val words = tokens
    def yylex(input:String):List[Token]
}

trait Token {
    
}