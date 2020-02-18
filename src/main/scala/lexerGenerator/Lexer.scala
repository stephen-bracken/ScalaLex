package lexerGenerator
class Lexer(rules: List[LexRule],private var _in:List[Char]){
    //def yylex(input:String):List[Any]
    private var _out:List[Char] = Nil
    private var _c:Char = _in.head
    //## IO methods ##
    /** gets the next character fron the input stream */
    def input = _c
    /** writes a character to the output stream */
    def output(c: Char) = {_out = c::_out}
    /** writes a character to the input stream */
    def unput(c: Char) = {_in = c::_in}
    
    //def reject

}

class Token(val name:String, val args:List[Any]) {
}

class LexerError(msg: String) extends Error(msg){}