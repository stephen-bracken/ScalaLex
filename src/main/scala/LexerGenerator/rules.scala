package lexerGenerator
import scala.util.parsing.combinator.RegexParsers

trait rule extends RegexParsers{
    val rule: Parser[Any]
    def parse = {
        
    }
}