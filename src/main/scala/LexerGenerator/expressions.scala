package lexerGenerator
import scala.util.Try
import scala.collection.immutable.Nil
import scala.language.postfixOps

object expressions {
  //def program: Parser[Any] = definitions ~ "%%" ~ rules ~ "%%" ~ routines
  def translateRegex(r: String) = {
    var stack: List[NFA] = List()
    var opStack: List[Char] = List()
    def epsilon: Char = '\u0000'
    def backspace: Char = '\u0008'
    val illegal: List[Char] = List(epsilon, backspace)
    val special: List[Char] = List('|', '*', '+', '(', ')', epsilon, backspace)
    var nextId: Int = 0

    def concatExpand(s: String): List[Char] = {
      def checkLeft(s: List[Char]): List[Char] = s match {
        case Nil       => Nil
        case ')' :: xs => ')' :: checkRight(xs)
        case '*' :: xs => '*' :: checkRight(xs)
        case x :: xs =>
          if (!isInput(x)) x :: checkLeft(xs)
          else x :: checkRight(xs)
      }
      def checkRight(s: List[Char]): List[Char] = {
        if (s isEmpty) Nil
        else {
          val c = s.head
          if (isInput(c) || c == '(') backspace :: checkLeft(s)
          else s
        }
      }
      checkLeft(s.toList)
    }
    // create an NFA from a regular expression string, return success or failure, store result on operatorStack
    def translateToNFA(s: List[Char]): (NFA, Boolean) = {
      //stack = List()
      //opStack = List()
      def translateAction(c: Char): Boolean = {
        println("translating '" + c + "'")
        ///add a state pair to represent a standard input symbol
        def parenth: Boolean = {
          if (opStack.head != '(')
            if (eval) parenth
            else false
          else {
            opStack = opStack.tail
            true
          }
        }
        if (isInput(c)) {
          push(c); true
        } else if (opStack.isEmpty) {
          opStack = c :: opStack; true
        } else if (c == '(') {
          opStack = c :: opStack; true
        } else if (c == ')') parenth
        else {
          if (!isOperator(c)) false
          while (!opStack.isEmpty && precedence(c, opStack.head)) {
            if (!eval) false
          }
          opStack = c :: opStack
          if (stack isEmpty) false
        }
        true
      }
      if (s.isEmpty) {
        //eval remaining operators
        if ((for (op <- opStack) yield eval).exists(x => x == false)) false
        val fsa = stack.head
        //add the final state as an accepting state
        fsa.addAccepting(fsa.getStates.last)
        (fsa, true)
      } else {
        val t = translateAction(s.head)
        if (!t) (null, false)
        else translateToNFA(s.tail)
      }
    }
    def isInput(c: Char) = !isOperator(c)
    def isOperator(c: Char) = special.contains(c)
    //enforces operator precedence
    def precedence(l: Char, r: Char) = {
      if (l == r) true
      else if (l == '*') false
      else if (r == '*') true
      else if (l == backspace) false
      else if (r == backspace) true
      else if (l == '|') false
      true
    }

    def eval: Boolean = {
      //evaluating operators and executing the correct action
      if (opStack isEmpty) false
      else {
        val o = opStack.head
        opStack = opStack.tail
        o match {
          case '*'      => star
          case '|'      => union
          case '+'      => concat; star
          case '\u0008' => concat
          case _        => false
        }
      }
    }

    def push(c: Char): Unit = {
      val s0 = new NFAState(nextId)
      val s1 = new NFAState(nextId + 1)
      nextId = nextId + 2
      s0.addTransition(c, s1)
      stack = (new NFA(List(s0, s1))) :: stack
    }
    def pop: (NFA, Boolean) = {
      if (stack isEmpty) (null, false)
      else {
        val p = stack.head
        stack = stack.tail
        (p, true)
      }
    }

    //translate concatenation (__)
    def concat: Boolean = {
      println("concat")
      val (b, t1) = pop
      val (a, t2) = pop
      if (!t1 || t2) false
      else
        //add epsilon transition from the final state of A to the initial state of B
        a.getStates.last.addTransition(epsilon, b.initialState)
      stack = (new NFA(a.getStates ++ b.getStates)) :: stack
      true
    }

    //translate kleene star (*)
    def star: Boolean = {
      println("star *")
      //pop one result off the stack
      val (a, t) = pop
      if (!t) false
      else {
        val s0 = new NFAState(nextId)
        val s1 = new NFAState(nextId + 1)
        val fst = a.initialState
        val lst = a.getStates.last
        nextId = nextId + 2

        //create transition from s0 to s1
        s0 addTransition (epsilon, s1)
        //create transition from s0 to the initial state of A
        s0 addTransition (epsilon, fst)
        lst addTransition (epsilon, s1)
        lst addTransition (epsilon, fst)
        stack = (new NFA(s0 :: a.getStates ++ List(s1))) :: stack
        true
      }
    }

    ///translate union (|)
    def union: Boolean = {
      println("union |")
      //pop two sub-results A and B
      val (b, t1) = pop
      val (a, t2) = pop
      if (!t1 || !t2) false
      else {
        val s0 = new NFAState(nextId)
        val s1 = new NFAState(nextId + 1)
        val fstA = a.initialState
        val fstB = b.initialState
        val lstA = a.getStates.last
        val lstB = b.getStates.last
        nextId = nextId + 2

        //create epsilon transition from s0 to the initial states of A and B
        s0 addTransition (epsilon, fstA)
        s0 addTransition (epsilon, fstB)
        //create epsilon transition from the final states of A and B to s1
        lstA addTransition (epsilon, s1)
        lstB addTransition (epsilon, s1)

        //create new FSAs with s1 and s0
        val newB = new NFA(b.getStates ++ List(s1))
        val newA = new NFA(s0 :: a.getStates)

        //add to the result set
        stack = (new NFA(newA.getStates ++ newB.getStates)) :: stack
        true
      }
    }

    //gets all the epsilon transitions for a single state

    def epsilonClosure(t: Set[NFAState]): Set[NFAState] = {
      var result = t.toList
      var unprocessed = t.toList
      while(!(unprocessed isEmpty)){
      val fst = t.head
      unprocessed = unprocessed tail
      val epsilons = fst transition(epsilon)
      for {u <- epsilons if !result.contains(u)} yield {result = u::result; unprocessed = u::unprocessed}
      }
      result.toSet
    }

    def dTranslate(s: NFAState,a: Set[NFAState]): DFA = {
      var dfaStates: List[DFAState] = List()
      nextId = 0
      println("dTranslate")
      //starting state of DFA is epsilon closure of first state of NFA
      val dfaStartState = new DFAState(epsilonClosure(Set(s)),nextId)
      var unmarked = List(dfaStartState)
      var processing:DFAState = dfaStartState
      var result:List[DFAState] = List(dfaStartState)
      while (!(unmarked isEmpty)){
        processing = unmarked.head
        unmarked = unmarked.tail
        for{c <- processing.inputSymbols
            } yield {
              val move = processing nfaMove c
              val closure = epsilonClosure(move)
              if(!(result exists(x => x.nfaStates == closure)))
              {
                nextId += 1
                val state = new DFAState(closure,nextId)
                result = state :: result
                unmarked = state :: unmarked
              }
              else{

              }
            }
      }
      new DFA(result)
    }
  
    if (!translateToNFA(concatExpand(r))._2)
      throw new FSAError("failed to parse regex")
    if (stack isEmpty) throw new FSAError("no NFA found")
    val nfa = stack.head
    //println("Included states: " + (for (s <- stack.head.states) yield s.id))
    if (!stack.tail.isEmpty)
      throw new FSAError(
        "unresolved states: " +
          (for {
            f <- stack.tail
            s <- f.getStates
          } yield s.id)
      )
    val d = dTranslate(nfa initialState, nfa accepting)
    println("included states" + d.getStates)
    println("accepting states: " + d.accepting)
    d
  }
}

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
