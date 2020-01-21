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
  var currentState:State = s.head
  var accepting:Set[A] = (for {s <- states if s.accepting} yield s).toSet
  val initialState: A = states.last
  var finalState:A = states.head
  def eval(s: String): Boolean = {
    currentState = initialState
    println("eval state " + currentState.id + " with '" + s.head + "'")
    def e(s:String):Boolean = {
    if (s isEmpty) currentState.accepting
    else {
      if (!(currentState.transitions.exists(x => x._1 == s.head))) false
      else {
          currentState = currentState.transition(s.head).head
          e(s.tail)
        }
      }
    }
    e(s)
  }

  def getStates = states
  def addState(s: A) = {
    states = s :: states
    finalState = s
  }
  def addStates(s:List[A]):Unit = {
    if(!s.isEmpty){
    addState(s.last)
    addStates(s.tail)
    }
  }

  def addAccepting(s: A) = {
    s accepting = true
    accepting = (s :: (accepting).toList).toSet
  }

}

class NFAState(i: Int, var accepting: Boolean = false) extends State {
  override val id: Int = i
  override type S = NFAState
  override var transitions: Map[Char, Set[NFAState]] = Map('\u0000' -> Set(this))//.withDefaultValue(Set())
  override def getTransitions(c: Char): Map[Char,Set[NFAState]] = transitions filter(t => t._1 == c)
  /*def removeEpsilon = {
    transitions = transitions filter (t => t._1 != '\u0000')
  }*/
    override def transition(c: Char): Set[NFAState] = (transitions(c).union(transitions('\u0000')))
    def addTransition(c: Char, s:NFAState) = {
    println(
      "adding transition (" +
        (c match {
          case '\u0000' => "epsilon"
          case '\u0008' => "backspace"
          case x        => x
        })
        + ") from state " + id + " to state " + s.id
    )
    //println("adding transition from state " + id + " to state " + s.id + " via " + c)
    if(transitions exists(x => x._1 == c)){
      transitions = transitions.updated(c, transitions(c).union(Set(s)))
    }
    else
    transitions = transitions.updated(c,Set(s))
  }
}

class DFAState(val nfaStates:Set[NFAState], override val id: Int) extends State{
  override type S = DFAState
  override var accepting = (nfaStates exists(p => p.accepting))
  override var transitions: Map[Char,Set[S]] = Map()//.withDefaultValue(Set())
  def included(s:NFAState) = nfaStates contains s
    //override def transition(c: Char): Set[DFAState] = transitions(c)
    def nextState(c: Char): List[DFAState] = transition(c).toList
    def addTransition(c: Char, s:DFAState) = {
    println(
      "adding transition (" +
        (c match {
          case '\u0000' => "epsilon"
          case '\u0008' => "backspace"
          case x        => x
        })
        + ") from state " + id + " to state " + s.id
    )
    //println("adding transition from state " + id + " to state " + s.id + " via " + c)
    transitions = transitions.updated(c,Set(s))

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

trait State{
  val id:Int
  type S <: State
  var accepting: Boolean
  var transitions: Map[Char,Set[S]]
  def getTransitions(c: Char) = transitions filter (t => t._1 == c)

  def transition(c: Char):Set[S] = transitions(c)
  override def toString(): String = {
    "state no. " + id
  }
}

class FSAError(msg: String) extends Error(msg)