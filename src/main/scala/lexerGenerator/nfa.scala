package scalaLex
import com.typesafe.scalalogging.LazyLogging

/** Represents a Non-deterministic Finite State Automata */
class NFA(s: List[NFAState]) extends FSA[NFAState](s) {
  //NFA Evaluation is not implemented but NFA and NFAStates are used in construction of DFAs
  @deprecated("evaluation of NFAs is not implemented","")
  override def eval(s: String): Boolean =  {logger.error("attempted evaluation on NFA");false}
}