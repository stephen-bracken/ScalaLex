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

    def epsilonClosure(t: List[State], r: List[State]): List[State] = {
      def getEpsilons(st: State) = {
        if (st.transitions.contains(epsilon)) {
          st transition epsilon filter (s => !r.contains(s))
        } else List()
      }
      var stack = t
      if(stack isEmpty) r
      else
      {
      var result = r
      val s = stack.head
      stack = stack.tail
        for(u <- getEpsilons(s) if(!r.contains(u))) yield {stack = u::stack;result = u::result}
        epsilonClosure(stack, result)
      }
    }

    def dTranslate(s: State,a: Set[State]) = {
      println("dTranslate")
      val dstates = epsilonClosure(List(s), List(s))
      def dTran(u: List[State], m: List[State]): List[State] = {
        if (u isEmpty) m
        else {
          val i = u.head
          val e = (for {
            s <- i.getSymbols
            e <- epsilonClosure(List(i.transition(s).head), List())
            if (!dstates.contains(e))
          } yield { i.addTransition(s, e); e })
          dTran(u.tail ++ e, u.head :: m)
        }
      }
      new DFA(dTran(dstates,List()),a)
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
            s <- f.states
          } yield s.id)
      )
    val d = dTranslate(nfa initialState, nfa accepting)
    println("included states" + d.states)
    println("accepting states: " + d.accepting)
    d
  }
}

class NFA(s:List[NFAState], accept: Set[NFAState] = Set()) extends FSA {
  override var states: List[NFAState] = states
  override var accepting: Set[NFAState] = accept
  override var currentState: NFAState = states.head
  override val initialState: NFAState = states.head
  override def getStates: List[NFAState] = states
  override def addAccepting(s: NFAState): Unit = {accepting = accepting.union(Set(s))}
  override def addState(s: NFAState): Unit = {states = s::states}
}

class DFA(states: List[DFAState], accept: Set[DFAState] = Set())
    extends FSA {
  
  //for (s <- states) yield { s removeEpsilon }
}

trait FSA {
  var states:List[State]
  var currentState:State
  def eval(s: String): Boolean = {
    currentState = states.head
    def e(s:String):Boolean = {
    if (s isEmpty) true
    else {
      if (!currentState.transitions.contains(s.head)) false
      else {
        val t = currentState.transitions(s.head)
          currentState = t.head
        e(s.tail)
      }}
    }
    e(s)
  }

  def getStates = states
  def addState(s: State) = {states = s :: states}

  def addAccepting(s: State) = {
    s accepting = true
    accepting = (s :: (accepting).toList).toSet
  }
  val initialState: State = states.head
  var accepting:Set[State]
}

class NFAState(val id: Int, var accepting: Boolean = false) extends State {
    val inputSymbols: Set[Char] =
    (for {c <- getSymbols } yield c).toSet
  override var transitions: Map[Char, List[NFAState]] = Map(/*'\u0000' -> List(this)*/)
  override def getTransitions(c: Char): Map[Char,List[NFAState]] = transitions filter(t => t._1 == c)
  /*def removeEpsilon = {
    transitions = transitions filter (t => t._1 != '\u0000')
  }*/
  override def transition(c: Char): List[NFAState] = transitions(c)
}

class DFAState(val nfaStates:List[NFAState], val id: Int) extends State{
  val accepting = !(nfaStates filter(p => p.accepting) isEmpty)
  def included(s:State) = nfaStates contains s
  override val inputSymbols: Set[Char] = 
    (for {s <- nfaStates
          c <- s inputSymbols} yield (c)).toSet
  override var transitions: Map[Char,List[DFAState]]
}

trait State{
  val id: Int
  var accepting: Boolean
  var inputSymbols: Set[Char]
  var transitions: Map[Char,List[State]]
  def getTransitions(c: Char) = transitions filter (t => t._1 == c)
  def addTransition(c: Char, s: State) = {
    /*println(
      "adding transition (" +
        (c match {
          case '\u0000' => "epsilon"
          case '\u0008' => "backspace"
          case x        => x
        })
        + ") from state " + id + " to state " + s.id
    )*/
    transitions = transitions.updated(c, List(s))
  }
  def getTransitions(c: Char) = transitions filter (t => t._1 == c)
  def transition(c: Char) = transitions(c)
  def getSymbols: List[Char] = (transitions keys).toList
  def removeEpsilon = {
    transitions = transitions filter (t => t._1 != '\u0000')
  }
  override def toString(): String = {
    "state no. " + id + ", transition symbols:" + getSymbols.toString
  }
}

class FSAError(msg: String) extends Error(msg)
