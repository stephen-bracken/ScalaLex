package lexerGenerator
import scala.language.postfixOps

class NFA(s:List[NFAState]) extends FSA[NFAState](s) {
}

class DFA(states: List[DFAState])
    extends FSA[DFAState](states) {
      
  //for (s <- states) yield { s removeEpsilon }
}

abstract class FSA[A<:State](s:List[A]) {
  var states:List[A] = s
  var currentState:A = s.head
  var accepting:Set[A] = (for {s <- states if s.accepting} yield s).toSet
  val initialState: A = states.head
  def eval(s: String): Boolean = {
    currentState = states.head
    def e(s:String):Boolean = {
    if (s isEmpty) true
    else {
      if (!currentState.transitions.contains(s.head)) false
      else {
        val t:List[A] = currentState.transition(s.head).toList
          currentState = t.head
        e(s.tail)
        }
      }
    }
    e(s)
  }

  def getStates = states
  def addState(s: A) = {states = s :: states}

  def addAccepting(s: A) = {
    s accepting = true
    accepting = (s :: (accepting).toList).toSet
  }

}

class NFAState(id: Int, var accepting: Boolean = false) extends State(id) {
  override type S = NFAState
  override var transitions: Map[Char, Set[S]] = Map(/*'\u0000' -> List(this)*/).withDefaultValue(Set())
  override var inputSymbols: Set[Char] = (for {c <- getSymbols } yield c).toSet
  override def getTransitions(c: Char): Map[Char,Set[NFAState]] = transitions filter(t => t._1 == c)
  /*def removeEpsilon = {
    transitions = transitions filter (t => t._1 != '\u0000')
  }*/
    def transition(c: Char): Set[NFAState] = transitions(c)
    def addTransition(c: Char, s:NFAState) = {
    if(transitions exists(x => x._1 == c)){
      transitions = transitions.updated(c, transitions(c).union(Set(s)))
    }
    else
    transitions = transitions.+(c -> Set(s))
    /*println(
      "adding transition (" +
        (c match {
          case '\u0000' => "epsilon"
          case '\u0008' => "backspace"
          case x        => x
        })
        + ") from state " + id + " to state " + s.id
    )*/
  }
}

class DFAState(val nfaStates:Set[NFAState], id: Int) extends State(id){
  override type S = DFAState
  override var accepting = (nfaStates exists(p => p.accepting))
  override var transitions: Map[Char,Set[S]] = Map()
  def included(s:NFAState) = nfaStates contains s
  override var inputSymbols: Set[Char] = 
    (for {s <- nfaStates
          c <- s inputSymbols} yield (c)).toSet
    def transition(c: Char): DFAState = transitions(c).head
    def addTransition(c: Char, s:DFAState) = {
    transitions = transitions.+(c -> Set(s))
    /*println(
      "adding transition (" +
        (c match {
          case '\u0000' => "epsilon"
          case '\u0008' => "backspace"
          case x        => x
        })
        + ") from state " + id + " to state " + s.id
    )*/
  }
  def nfaMove(c:Char):Set[NFAState] = {
    def move(s: List[NFAState]):Set[NFAState] = {
      if(s isEmpty) Set()
      else
      s.head transition(c) union(move(s tail))
    }
    move(nfaStates.toList)
  }
}

abstract class State(val id:Int){
  type S <: State
  var accepting: Boolean
  var inputSymbols: Set[Char]
  var transitions: Map[Char,Set[S]]
  def getTransitions(c: Char) = transitions filter (t => t._1 == c)

  def transition[A<:State](c: Char):List[A] = transitions(c).asInstanceOf[List[A]]
  def getSymbols: List[Char] = (transitions keys).toList
  override def toString(): String = {
    "state no. " + id + ", transition symbols:" + getSymbols.toString
  }
}

class FSAError(msg: String) extends Error(msg)