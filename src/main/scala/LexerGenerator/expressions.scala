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
        a.getStates.head.addTransition(epsilon, b.initialState)
      stack = a :: stack
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

    //Subset construction algorithm
    //gets all the epsilon transitions for a single state

    def epsilonClosure(t: List[NFAState]): Set[NFAState] = {
      println("epsilon closure")
      var result = t.toList
      var unprocessed = t.toList
      while(!(unprocessed isEmpty)){
        val fst = t.head
        unprocessed = unprocessed tail
        val epsilons = fst transition(epsilon)
        for {u <- epsilons if !result.contains(u)} yield {result = u::result; unprocessed = u::unprocessed}
      }
      println(result)
      result.toSet
    }

    def dTranslate(s: NFAState,a: Set[NFAState]): DFA = {
      var dfaStates: List[DFAState] = List()
      nextId = 0
      println("dTranslate")
      //starting state of DFA is epsilon closure of first state of NFA
      val dfaStartState = new DFAState(epsilonClosure(List(s)),nextId)
      var unmarked = List(dfaStartState)
      var processing:DFAState = dfaStartState
      var result:List[DFAState] = List(dfaStartState)
      while (!(unmarked isEmpty)){
        processing = unmarked.head
        unmarked = unmarked.tail
        for{c <- processing.transitions.keys
            } yield {
              val move = processing nfaMove c
              val closure = epsilonClosure(move.toList)
              if(!(result exists(x => x.nfaStates == closure)))
              {
                nextId += 1
                val state = new DFAState(closure,nextId)
                processing.addTransition(c,state)
                result = state :: result
                unmarked = state :: unmarked
              }
              else{
                val res = result.head
                res.addTransition(c,processing)
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
    nfa
  }
}
