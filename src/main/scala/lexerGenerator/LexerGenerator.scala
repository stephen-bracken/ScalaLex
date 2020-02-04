package lexerGenerator

import com.typesafe.scalalogging.LazyLogging
import java.io.FileNotFoundException
import scala.io.Source


object Generator extends LazyLogging{
    def main(args: Array[String]): Unit = {
        try{
            val currentDirectory = new java.io.File(".").getCanonicalPath
            val inputFile = args(0)
            logger.info("reading definitions from " + currentDirectory + '/' + inputFile)
            val outputFile = args(1)
            val bufferedSource = Source.fromFile(inputFile)
            var lines:List[String] = List()
            for (line <- bufferedSource.getLines) {
                lines = line :: lines
                logger.trace(line)
            }
            bufferedSource.close
            lines = lines.reverse
            logger.trace("final text: " + lines)
            logger.info("writing output to " + outputFile)
        }
        catch {
            case e:ArrayIndexOutOfBoundsException => usage
            case e:FileNotFoundException => throw new GeneratorError(e.getMessage())
        }
    }
    def ReadRules(rules:List[String]):Lexer = ???
    def readTransition() = ???
    def usage = {
        logger.error("Usage: LexerGenerator [Definitions file] [Output filename]")
    }
}

class GeneratorError(msg: String) extends Error(msg) {

}