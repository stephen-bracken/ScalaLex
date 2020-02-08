package lexerGenerator
import scala.language.postfixOps
import com.typesafe.scalalogging.LazyLogging

/** Represents a Non-deterministic Finite State Automata */
class NFA(s:List[NFAState]) extends FSA[NFAState](s) {
  //NFA Evaluation is not implemented but NFA and NFAStates are used in construction of 
  @deprecated("evaluation of NFAs is not implemented","")
  override def eval(s: String): Boolean =  {logger.error("attempted evaluation on NFA");false}
}

/** Represents a Deterministic Finite State Automata */
class DFA(s: List[DFAState],val regex:String)
    extends FSA[DFAState](s) {
    /** evaluates an input string against this DFA. Returns true if input string results in an accepting state */
    override def eval(s: String): Boolean = {
      def e(s:String,st:DFAState):Boolean = {
        if (s isEmpty) st.accepting
        else {
          logger.trace("evaluating symbol "+s.head+" in "+st)
          if (!(st.transitions.exists(x => x._1 == s.head))) false
          else {
            val next = st.nextState(s.head)
            logger.trace("transition to "+next)
            e(s.tail,next)
          }
        }
      }
      logger.debug("DFA evaluating string " + s)
      e(s,initialState)
    }
}

/**
  * Represents a Finite State Automata
  *
  * @param s List of states in order
  */
abstract class FSA[A<:State](s:List[A]) extends LazyLogging {
  /** the set of states in this FSA */
  private var states:List[A] = s
  /** the set of accepting states in this FSA */
  var accepting:Set[A] = (for {s <- states if s.accepting} yield s).toSet
  /** the starting state for this FSA */
  val initialState: A = states.last
  /** the last state in this FSA */
  var finalState:A = states.head

  def getStates = states

  /** gets a state from this FSA by id */
  def getState(i: Int):A = states.find(a => a.id == i) match {
    case None => throw new FSAError("State not found: " + i)
    case Some(v) => v
  }

  /** adds a state to this FSA */
  def addState(s: A) = {
    states = s :: states
    finalState = s
  }

  /** adds a series of states to this FSA */
  def addStates(s:List[A]):Unit = {
    def add(s:List[A]):Unit = {
    if(!s.isEmpty){
    addState(s.last)
    add(s.tail)
    }
    }
    add(s.reverse)
  }

  /** adds a given state to the set of accepting states for this FSA */
  def addAccepting(s: A) = {
    s accepting = true
    accepting = accepting.union(Set(s))
  }

  /** checks an input string against this FSA */
  def eval(s:String):Boolean
  /** removes a state from this FSA */
  def removeState(s: A) = {
    states = states.filter(x => x != s)
  }
}

class NFAState(id: Int, var accepting: Boolean = false) extends State (id){
  override type S = NFAState
  override var transitions: Map[Char, Set[NFAState]] = Map('\u0000' -> Set(this))//.withDefaultValue(Set())
  //override def getTransitions(c: Char): Map[Char,Set[NFAState]] = transitions filter(t => t._1 == c)
  /*def removeEpsilon = {
    transitions = transitions filter (t => t._1 != '\u0000')
  }*/
  override def transition(c: Char): Set[NFAState] = if (transitions contains(c)) transitions(c) else Set()

}

class DFAState(n:Set[NFAState] = Set(), id: Int) extends State(id){
  /** the set of NFAStates that this DFAState was constructed from */
  val nfaStates = n
  override type S = DFAState
  override var accepting = (nfaStates exists(p => p.accepting))
  override var transitions: Map[Char,Set[S]] = Map()//.withDefaultValue(Set())
  def included(s:NFAState) = nfaStates contains s
  /** gets the next DFAState using this transition symbol */
  def nextState(c: Char): DFAState = transition(c).head
  /** sets the transition from this state via c to s */
  override def addTransition(c: Char, s:DFAState) = {
    logger.trace(
      "adding DFA transition (" +
        (c match {
          case '\u0000' => "epsilon"
          case '\u0008' => "backspace"
          case x        => x
        })
        + ") from state " + id + " to state " + s.id
    )
    transitions = transitions.updated(c,Set(s))
  }
  /** gets all of the visible NFAStates using a single transition on a character input*/
  def nfaMove(c:Char):Set[NFAState] = {
    def move(s: List[NFAState]):Set[NFAState] = {
      if(s isEmpty) Set()
      else
      s.head transition(c) union(move(s tail))
    }
    move(nfaStates.toList)
  }
  /** checks if the State is a dead end */
  def deadEnd:Boolean = {
       if(accepting) false
       else
       if(transitions.keySet.isEmpty) true
       else !(transitions.exists(p => !(p._2.diff(Set(this)).isEmpty)))
  }
}

abstract class State(val id:Int) extends LazyLogging{
  type S <: State
  /** indicates whether or not this state is an accepting state in the FSA */
  var accepting: Boolean
  /** a map of state transitions using valid symbols */
  var transitions: Map[Char,Set[S]]

  /** yields the possible state transitions from a given character */
  def transition(c: Char):Set[S] = transitions(c)
  def addTransition(c: Char, s:S) = {
    logger.trace("adding transition from "+this+" to "+s+" via '"+(c match {
          case '\u0000' => "epsilon"
          case '\u0008' => "backspace"
          case x        => x
        })+'\'')
    if(transitions exists(x => x._1 == c)){
      transitions = transitions.updated(c, transitions(c).union(Set(s)))
    }
    else transitions = transitions.updated(c,Set(s))
  }
  /** removes all of the transitions from this state to s*/
  def removeTransitions(s: S) = {for ((k,v) <- transitions) yield {transitions = transitions.updated(k,v.diff(Set(s)))}}
  override def toString(): String = {
    "state " + id
  }
  override def equals(x: Any): Boolean = id == x.asInstanceOf[S].id
}

class FSAError(msg: String) extends Error(msg) with LazyLogging{
  logger.error(msg)
}