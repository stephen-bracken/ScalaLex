package lexerGenerator
import scala.language.postfixOps
import com.typesafe.scalalogging.LazyLogging
import scala.annotation.tailrec
import scala.collection.immutable.Nil

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
      @tailrec
      def e(s:String,st:DFAState):Boolean = {
        if (s isEmpty) st.accepting
        else {
          logger.trace("evaluating symbol "+s.head+" in "+st)
          if (!(st.transitions.exists(x => x.makeTransition(s.head)))) false
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
    /** evaluates an input string and all of its substrings */
    def getMatches(in:String) = {
      (for {
        s <- 0 to in.length
        e <- s to in.length 
      } yield {
        val str = in.substring(s,e)
      (s,e,str,eval(str))}).filter(p => p._4).map(f => (f._1,f._2,f._3)).toList
    }
    /** returns whether the input or any substring matches against this automata */
    def anyMatches(in:String) = {
      !getMatches(in).isEmpty
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
    @tailrec
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
  /** a list of epsilon transitions from this state */
  var epsilons:Set[NFAState] = Set(this)
  /** adds an epsilon transition from this state to s */
  def epsilons_(s: NFAState) = epsilons = epsilons.union(Set(s))
}

class DFAState(n: Set[NFAState] = Set(), id: Int) extends State(id){
  /** the set of NFAStates that this DFAState was constructed from */
  val nfaStates = n
  override type S = DFAState
  override var accepting = (nfaStates exists(p => p.accepting))
  /** indicates whether the nfaState is included in the epsilon closure of this DFAState */
  def included(s:NFAState) = nfaStates contains s
  /** gets the next DFAState using this transition symbol */
  def nextState(c: Char): DFAState = transition(c).head
  /** gets all of the visible NFAStates using a single transition on a character input*/
  def nfaMove(c:Char,i:Boolean):Set[NFAState] = {
    @tailrec
    def move(s: List[NFAState],a: Set[NFAState]):Set[NFAState] = {
      if(s isEmpty) a
      else
      s.head.transitions.find(p => p.chars.contains(c)&&p.inverted==i) match {
        case Some(v) => move(s.tail,v.result.union(a))
        case None => move(s.tail,a)
      }
    }
    move(nfaStates.toList,Set())
  }
  /** special case NFA traversal with inverted empty character sets i.e. [^] */
  def emptyNFAMove:Set[NFAState] = {
    @tailrec
    def move(s: List[NFAState],a: Set[NFAState]):Set[NFAState] = {
      if(s isEmpty) a
      else
      s.head.transitions.find(p => p.chars.isEmpty&&p.inverted) match {
        case Some(v) => move(s.tail,v.result.union(a))
        case None => move(s.tail,a)
      }
    }
    move(nfaStates.toList,Set())
  }
  /** checks if the State is a dead end */
  def deadEnd:Boolean = {
       if(accepting) false
       else
       if(transitions.isEmpty) true
       else !(transitions.exists(p => !(p.result.diff(Set(this)).isEmpty)))
  }
}

abstract class State(val id:Int) extends LazyLogging{
  type S <: State
  /** indicates whether or not this state is an accepting state in the FSA */
  var accepting: Boolean
  /** a set of possible Transition encodings from this state */
  var transitions: Set[Transition[S]] = Set()

  /** yields the possible state transitions from a given character */
  def transition(c: Char):Set[S] = transitions.find(x => x.makeTransition(c)) match {
    case Some(t) => 
      //logger.trace("found transition " + t._2)
      t.result
    case None => Set()
  }
  /** adds a transition or an inverse transition from this state to s via the chars in c */
  def addTransition(i:Boolean, s:S,c: Char*) = {
    val chars = c.toSet
    logger.trace("adding transition from "+this+" to "+s+" via '" + (i match {
      case false => chars
      case true => "not " + chars
    }))
    transitions.find(p => (p.chars == chars) && (p.inverted == i)) match {
      case Some(v) => v.addState(s)
      case None => 
        transitions.find(p => p.inverted == i && p.result.contains(s)) match {
          case Some(v) => 
            v.addCharacters(c.toList:_*)
          case None =>         
            val t = new Transition[S](chars,i,Set(s))
            logger.trace("creating new state transition: " + t)
            transitions = transitions.union(Set(t))
        }
    }
  }
  /** creates a special case transition representing [^] */
  def addEmptyTransition(s: S) = {
    transitions.find(p => p.chars.isEmpty && p.inverted) match {
      case Some(v) => v.addState(s)
      case None => transitions = transitions.union(Set(new Transition[S](Set(),false,Set(s))))
    }
  }
  /** removes all of the transitions from this state to s*/
  def removeTransitions(s: S) = {for (t <- transitions) yield {t.result = t.result.diff(Set(s))}}
  override def toString(): String = {
    "state " + id
  }
  override def equals(x: Any): Boolean = id == x.asInstanceOf[S].id
}

/** represents a transition from this state. Contains associated characters, whether the transition is inverted (^), and the set of possible destinations */
case class Transition[S<:State](var chars: Set[Char],val inverted: Boolean = false,var result: Set[S]) extends LazyLogging{
  def makeTransition(c: Char):Boolean = chars.contains(c) ^ inverted
  def addState(s:S) = {result = result.union(Set(s))}
  def addCharacters(c:Char*) = {chars = chars.union(c.toSet)}
  override def toString():String = {
    chars.toString + ", inverted:" + inverted
  }
}

class FSAError(msg: String) extends Error(msg) with LazyLogging{
  logger.error(msg)
}