package lexerGenerator

import com.typesafe.scalalogging.LazyLogging
import scala.annotation.tailrec

object Util extends LazyLogging{

    def asString(l: List[Char]) = l.foldLeft("")((s,c) => s + c)

    def trimWhitespace(s: List[Char]):List[Char] = {
        logger.trace("formatting whitespace: " + s)
        def trim(s: List[Char],a: List[Char]):List[Char] = {
            s match {
                case Nil => a
                //case ' '::Nil => a
                case ' '::' '::xs =>
                    trim(xs,a :+ ' ')
                case x::xs =>
                    trim(xs,a :+ x)
            }
        }
        trim(s,Nil)
    }

    def convertToTabs(s : List[Char]) = { 
        @tailrec
        def convert(s: List[Char],a: String):String = s match {
            case Nil => a
            case ' ' :: ' ' :: ' ' :: ' ' :: ' ' :: ' ' :: ' ' :: ' ' :: xs =>
                convert(xs,a+'\t')
            case x :: xs =>
                convert(xs,a+x)
        }
        convert(s,"")
    }

    def indentString(n:Int):String = {
        (for(i <- 0 until n) yield ('\t')).foldLeft("")((s,c) => s + c)
    }

    /** removes leading whitespace from a character sequence */
    def trimLeading(l: List[Char]):List[Char] = l.dropWhile(c => c == ' ' || c == '\t')

    /** removes leading whitespace from a string */
    def trimLeading(s: String):String = s.dropWhile(c => c == ' ' || c == '\t')
}