package scalaLex
import scala.language.postfixOps
import com.typesafe.scalalogging.LazyLogging
import scala.annotation.tailrec
import scala.collection.immutable.Nil

/** Represents a Deterministic Finite State Automata */
class DFA(s: List[DFAState],val regex:String)
    extends FSA[DFAState](s) {
    
    def apply(s: String):Boolean = eval(s)
    /** evaluates an input string against this DFA. Returns true if input string results in an accepting state */
    override def eval(s: String): Boolean = {
      @tailrec
      def e(s: String,st: DFAState):Boolean = {
        if (s isEmpty) {val a = st.accepting
          a match {
            case true =>
              logger.trace("DFA success:" + s)
            case false =>
              logger.trace("DFA failure:" + s)
          }
          a
        }
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
    /**
      * matches this dfa against an input string and all substrings
      *
      * @param in input string
      * @return (Start pos, End pos, matched text)
      */
    def getMatches(in: String):List[(Int,Int,String)] = {
      (for {
        s <- 0 to in.length
        e <- s to in.length 
      } yield {
        val str = in.substring(s,e)
      (s,e,str,eval(str))}).filter(p => p._4).map(f => (f._1,f._2,f._3)).toList
    }
    /** returns whether the input or any substring matches against this automata */
    def anyMatches(in: String):Boolean = {
      !getMatches(in).isEmpty
    }
    /**
      * Gets the longest prefix match in the list from getMatches
      *
      * @param in input string
      * @return (match length, matched text)
      */
    def longestPrefixMatch(in: String):(Int,String) = {
      (for {
        i <- 0 to in.length
      } yield {
        val str = in.substring(0,i)
        (i,str,eval(str))
      }).filter(x => x._3).map(x => (x._1,x._2)).last
    }
}

/**
  * Represents a Finite State Automata
  *
  * @param s List of states in order
  */
abstract class FSA[A<:State](private var _s:List[A]) extends LazyLogging {
  /** gets the set of states in this FSA */
  def states = _s
  /** the starting state for this FSA */
  val initialState: A = _s.last
  /** the last state in this FSA */
  var finalState:A = states.head

  var startMatch:Boolean = false
  var endMatch:Boolean = false
  /** gets a state from this FSA by id */
  def getState(i: Int):A = states.find(a => a.id == i) match {
    case None => throw new FSAError("State not found: " + i)
    case Some(v) => v
  }

  /** adds a state to this FSA */
  def addState(s: A) = {
    _s = s :: _s
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
  }

  /** checks an input string against this FSA */
  def eval(s:String):Boolean
  /** removes a state from this FSA */
  def removeState(s: A) = {
    _s = _s.filter(x => x != s)
  }
}

class DFAState(private val _n: Set[NFAState] = Set(), id: Int) extends State(id){
  /** gets the set of NFAStates that this DFAState was constructed from */
  def nfaStates = _n
  override type S = DFAState
  //if any state in _n is accepting, this state is an accepting state
  override var accepting = (_n exists(p => p.accepting))
  /** indicates whether the nfaState is included in the epsilon closure of this DFAState */
  def included(s:NFAState) = _n contains s
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
    move(_n.toList,Set())
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

class NFAState(id: Int, a: Boolean = false) extends State (id){
  override var accepting: Boolean = a
  override type S = NFAState
  private var _epsilons = Set(this)
  /** a list of epsilon transitions from this state */
  def epsilons = _epsilons
  /** adds an epsilon transition from this state to s */
  def epsilons_(s: NFAState) = _epsilons = _epsilons.union(Set(s))
}

abstract class State(private val _id:Int) extends LazyLogging{
  type S <: State
  /** gets the id of this state */
  def id = _id
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
    // search for transitions with the same inputs
    transitions.find(p => (p.chars == chars) && (p.inverted == i)) match {
      case Some(v) => v.addState(s)
      case None => 
        //search for transitions with the same outputs
        transitions.find(p => p.inverted == i && p.result.contains(s)) match {
          case Some(v) => 
            v.addCharacters(c.toList:_*)
          case None => 
            //no transitions found
            val t = new Transition[S](chars,i,Set(s))
            logger.trace("creating new state transition: " + t)
            transitions = transitions.union(Set(t))
        }
    }
  }
  /** creates a special case transition representing [^] */
  def addEmptyTransition(i:Boolean,s: S) = {
    transitions.find(p => p.chars.isEmpty && p.inverted == i) match {
      case Some(v) => v.addState(s)
      case None => transitions = transitions.union(Set(new Transition[S](Set(),i,Set(s))))
    }
  }
  /** removes all of the transitions from this state to s*/
  def removeTransitions(s: S) = {for (t <- transitions) yield {t.result = t.result.diff(Set(s))}}
  override def toString(): String = {
    "state " + id
  }
}

/** represents a transition from this state. Contains associated characters, whether the transition is inverted (^), and the set of possible destinations */
case class Transition[S<:State](var chars: Set[Char],val inverted: Boolean = false,var result: Set[S]) extends LazyLogging{
  /** indicates whether an input character represents an acceptible transition symbol */
  def makeTransition(c: Char):Boolean = chars.contains(c) ^ inverted
  /** adds a state to the result set */
  def addState(s:S) = {result = result.union(Set(s))}
  /** adds the character(s) to the input set */
  def addCharacters(c:Char*) = {chars = chars.union(c.toSet)}
  override def toString():String = {
    chars.toString + ", inverted:" + inverted
  }
}

class FSAError(msg: String) extends Error(msg) with LazyLogging{
  logger.error(msg)
}