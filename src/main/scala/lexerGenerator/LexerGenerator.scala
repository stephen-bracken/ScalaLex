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
            logger.info("writing output to " + outputFile)
            val bufferedSource = Source.fromFile(inputFile)
            for (line <- bufferedSource.getLines) {
                println(line.toUpperCase)
            }
            bufferedSource.close
        }
        catch {
            case e:ArrayIndexOutOfBoundsException => usage
            case e:FileNotFoundException => throw new GeneratorError(e.getMessage())
        }
    }
    def ReadRules(rules:String):Lexer = ???
    def readTransition() = ???
    def usage = {
        logger.error("Usage: LexerGenerator [Definitions file] [Output filename]")
    }
}

class GeneratorError(msg: String) extends Error(msg) {

}