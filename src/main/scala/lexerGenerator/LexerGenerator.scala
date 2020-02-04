package lexerGenerator

import com.typesafe.scalalogging.LazyLogging


object Generator extends LazyLogging{
    def main(args: Array[String]): Unit = {
        try{
        val inputFile = args(0)
        logger.info("reading definitions from " + inputFile)
        val outputFile = args(1)
        logger.info("writing output to " + outputFile)
        }
        catch {
            case e:Throwable => usage
        }
    }
    def ReadRules(rules:String):Lexer = ???
    def readTransition() = ???
    def usage = {
        logger.error("Usage: LexerGenerator [Definitions file] [Output filename]")
    }
}