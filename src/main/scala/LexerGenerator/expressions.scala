package lexerGenerator
import scala.util.Try
import scala.collection.immutable.Nil
import scala.language.postfixOps

object expressions {
  //def program: Parser[Any] = definitions ~ "%%" ~ rules ~ "%%" ~ routines
  def translateRegex(r: String) = {
    var stack: List[FSA] = List()
    var opStack: List[Char] = List()
    def epsilon: Char = '\u0000'
    def backspace: Char = '\u0008'
    val illegal: List[Char] = List(epsilon, backspace)
    val special: List[Char] = List('|', '*', '+', '(', ')', backspace)
    var nextId: Int = 0
    // create an NFA from a regular expression string, return success or failure, store result on operatorStack
    def translateToNFA(s: List[Char]): (FSA,Boolean) = {
      stack = List()
      opStack = List()
      def translateAction(c:Char):Boolean = {
        ///add a state pair to represent a standard input symbol
        def parenth: Boolean = {
          if (opStack.head != '(')
            if (eval) parenth
            else false
          else
            {
            opStack = opStack.tail
          true}
        }
        if(isInput(c)) {push(c) ; true}
        else if (opStack.isEmpty) {opStack = c :: opStack ; true}
        else if (c == '(') {opStack = c :: opStack ; true}
        else if (c == ')') parenth
        else {
          if(!isOperator(c)) false
          while(!opStack.isEmpty && precedence(c,opStack.head)){
            if(!eval) false
          }
          opStack = c :: opStack
          if (stack isEmpty) false
        }
        true
      }
      if(s.isEmpty) {
        //eval remaining operators
        if((for(op <- opStack) yield eval).exists(x => x == false)) false
        //error:head of empty list
        val fsa = stack.head
        //add the final state as an accepting state
        fsa.addAccepting(fsa.states.last)
        (fsa,true)}
      else {
        val t = translateAction(s.head)
        if(!t) (null,false)
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
      val s0 = new State(nextId + 1)
      val s1 = new State(nextId + 2)
      nextId = nextId + 2
      s0.addTransition(c, s1)
      stack = (new FSA(List(s0, s1))) :: stack
    }
    def pop: (FSA, Boolean) = {
      if (stack isEmpty) (null, false)
      else {
        val p = stack.head
        stack = stack.tail
        (p, true)
      }
    }


    //translate concatenation (__)
    def concat: Boolean = {
      val (b, t1) = pop
      val (a, t2) = pop
      if (!t1 || t2) false
      else
        //add epsilon transition from the final state of A to the initial state of B
        a.states.last.addTransition(epsilon, b.initialState)
      stack = (new FSA(a.states ++ b.states)) :: stack
      true
    }

    //translate kleene star (*)
    def star: Boolean = {
      //pop one result off the stack
      val (a, t) = pop
      if (!t) false
      else {
        val s0 = new State(nextId + 1)
        val s1 = new State(nextId + 2)
        val fst = a.initialState
        val lst = a.states.last
        nextId = nextId + 2

        //create transition from s0 to s1
        s0 addTransition (epsilon, s1)
        //create transition from s0 to the initial state of A
        s0 addTransition (epsilon, fst)
        lst addTransition (epsilon, s1)
        lst addTransition (epsilon, fst)
        stack =  (new FSA(s0 :: a.states ++ List(s1))) :: stack
        true
      }
    }

    ///translate union (|)
    def union: Boolean = {
      //pop two sub-results A and B
      val (b, t1) = pop
      val (a, t2) = pop
      if (!t1 || !t2) false
      else {
        val s0 = new State(nextId + 1)
        val s1 = new State(nextId + 2)
        val fstA = a.initialState
        val fstB = b.initialState
        val lstA = a.states.last
        val lstB = b.states.last
        nextId = nextId + 2

        //create epsilon transition from s0 to the initial states of A and B
        s0 addTransition (epsilon, fstA)
        s0 addTransition (epsilon, fstB)
        //create epsilon transition from the final states of A and B to s1
        lstA addTransition (epsilon, s1)
        lstB addTransition (epsilon, s1)

        //create new FSAs with s1 and s0
        val newB = new FSA(b.states ++ List(s1))
        val newA = new FSA(s0 :: a.states)

        //add to the result set
        stack = (new FSA(newA.states ++ newB.states)) :: stack
        true
      }
    }

    //gets all the epsilon transitions for a single state

    def epsilonClosure(t: List[State], r: List[State]): List[State] = {
      def getEpsilons(st: State) = {
        st transition epsilon filter (s => !r.contains(s))
      }
      if (t.isEmpty) r
      else {
        val e = getEpsilons(t.head)
        epsilonClosure(e ++ t.tail, r ++ e)
      }
    }

    def dTranslate(s: State) = {
      val dstates = epsilonClosure(List(s), List(s))
      def dTran(u: List[State], m: List[State]): List[State] = {
        if (u isEmpty) m
        else {
          val e = (for {
            s <- u.head.getSymbols
            e <- epsilonClosure(List(u.head transition (s) head), List())
            if (!dstates.contains(e))
          } yield { u.head addTransition (s, e); e })
          dTran(u.tail ++ e, u.head :: m)
        }
      }
      new DFA(dTran(dstates, List()))
    }

    if (!translateToNFA(r.toList)._2) throw new FSAError("failed to parse regex")
    if (stack isEmpty) throw new FSAError("no NFA found")
    val nfa = stack.head
    if (!stack.tail.isEmpty) throw new FSAError("unresolved states")
    dTranslate(nfa initialState)
  }
}

class FSA(
    val states: List[State],
    val accept: Set[State] = Set()
) {
  private var currentState = states.head
  def eval(s: String): Boolean = {
    if (s isEmpty) true
    else {
      val t = currentState.transitions(s.head)
      if (t isEmpty) false
      else
        currentState = t.head
      eval(s.tail)
    }
  }
  def addAccepting(s: State) = {
    accepting = (s::(accepting).toList).toSet
  }
  val inputSymbols:Set[Char] = (for {s <- states; c <- s getSymbols} yield c).toSet
  val initialState: State = states.head
  var accepting = accept
}

class DFA(states: List[State], accepting: Set[State] = Set())
    extends FSA(states, accepting) {
  for (s <- states) yield { s removeEpsilon }
}

class State(val id: Int) {
  var transitions: Map[Char, List[State]] = Map('\u0000' -> List(this))
  def addTransition(c: Char, s: State) = {
    println("adding transition ("+
    (c match {
      case '\u0008' => "epsilon" 
      case x => x }) 
      +") from state " + id + " to state " + s.id)
    transitions = transitions.updated(c, List(s))
  }
  def getTransitions(c: Char) = transitions filter (t => t._1 == c)
  def transition(c: Char) = transitions(c)
  def getSymbols:List[Char] = (transitions keys).toList
  def removeEpsilon = {
    transitions = transitions filter (t => t._1 != '\u0000')
  }
}

class FSAError(msg: String) extends Error(msg)
