package lexerGenerator
import scala.language.postfixOps

class NFA(s:List[NFAState]) extends FSA[NFAState](s) {
  //NFA Evaluation is not implemented but NFA and NFAStates are used in construction of DFAs
  override def eval(s: String): Boolean = false
}

class DFA(states: List[DFAState])
    extends FSA[DFAState](states) {
    override def eval(s: String): Boolean = {
      def e(s:String,st:DFAState):Boolean = {
        if (s isEmpty) st.accepting
        else {
          println("eval state " + st.id + " with '" + s.head + "'")
          if (!(st.transitions.exists(x => x._1 == s.head))) false
          else {
            val next = st.nextState(s.head)
            e(s.tail,next)
          }
        }
      }
      e(s,initialState)
    }
  //for (s <- states) yield { s removeEpsilon }
}

abstract class FSA[A<:State](s:List[A]) {
  var states:List[A] = s
  var accepting:Set[A] = (for {s <- states if s.accepting} yield s).toSet
  val initialState: A = states.last
  var finalState:A = states.head

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
    accepting = accepting.union(Set(s))
  }

  def eval(s:String):Boolean
}

class NFAState(id: Int, var accepting: Boolean = false) extends State (id){
  override type S = NFAState
  override var transitions: Map[Char, Set[NFAState]] = Map('\u0000' -> Set(this))//.withDefaultValue(Set())
  //override def getTransitions(c: Char): Map[Char,Set[NFAState]] = transitions filter(t => t._1 == c)
  /*def removeEpsilon = {
    transitions = transitions filter (t => t._1 != '\u0000')
  }*/
    override def transition(c: Char): Set[NFAState] = if (transitions contains(c)) transitions(c) else Set()
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

class DFAState(val nfaStates:Set[NFAState] = Set(), id: Int) extends State(id){
  override type S = DFAState
  override var accepting = (nfaStates exists(p => p.accepting))
  override var transitions: Map[Char,Set[S]] = Map()//.withDefaultValue(Set())
  def included(s:NFAState) = nfaStates contains s
    //override def transition(c: Char): Set[DFAState] = transitions(c)
    def nextState(c: Char): DFAState = transition(c).head
    def addTransition(c: Char, s:DFAState) = {
    println(
      "adding DFA transition (" +
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

abstract class State(val id:Int){
  type S <: State
  var accepting: Boolean
  var transitions: Map[Char,Set[S]]
  //def getTransitions(c: Char) = transitions filter (t => t._1 == c)

  def transition(c: Char):Set[S] = transitions(c)
  override def toString(): String = {
    "state " + id
  }
  override def equals(x: Any): Boolean = id == x.asInstanceOf[S].id
}

class FSAError(msg: String) extends Error(msg)