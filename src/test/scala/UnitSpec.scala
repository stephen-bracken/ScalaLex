package lexerGenerator
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time._
abstract class UnitSpec extends AnyFlatSpec with Matchers with
  OptionValues with Inside with Inspectors with TimeLimitedTests {
    override def timeLimit: Span = Span(10000,Millis)
    val backspace = '\u0008'
  }