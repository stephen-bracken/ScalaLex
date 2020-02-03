package lexerGenerator
import scala.util.Try
import scala.collection.immutable.Nil
import scala.language.postfixOps
import org.slf4j.LoggerFactory
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.core.util.StatusPrinter
import com.typesafe.scalalogging.LazyLogging

/**Thompson construction and subset construction implemented using shunting yard algorithm to produce a DFA from a given regular expression string
 */
object regexParser extends LazyLogging {

  //###### Logging ######
  //val logger = Logger(LoggerFactory.getLogger(expressions.getClass()))

  // assume SLF4J is bound to logback in the current environment
  private val lc:LoggerContext = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
  // print logback's internal status
  StatusPrinter.print(lc)
  //###### Character evaluation ######
  /** unicode for an empty character - used to represent an epsilon transition*/
  private val epsilon: Char = '\u0000'
  /** unicode for a backspace character - used to represent concatenation*/
  private val backspace: Char = '\u0008'
  /** the characters that are not allowed in the input string*/
  private val illegal: Set[Char] = Set(epsilon, backspace)
  /** chars that represent regex operators */
  private val operators: Set[Char] = Set('|', '*', '+', epsilon, backspace,'(',')','\\')

  /** checks if a character is an input or not */
  private def isInput(c: Char) = !isOperator(c)
  /** checks if a character is in the set of operators or not */
  private def isOperator(c: Char) = operators.contains(c)
  /** enforces operator precedence on the stack */
  private def precedence(l: Char, r: Char) = {
    val o = List('|','*','+')
    logger.trace("precedence '" + l + "' '" + r + '\'')
    if (l == r) true
    else if (o.contains(l)) false
    else if (r == '*'|| r == '|') true
    else if (l == '\\') false
    else if (l == backspace) false
    else if (r == backspace) true
    else true
  }
  
  //def program: Parser[Any] = definitions ~ "%%" ~ rules ~ "%%" ~ routines
  /**
    * Translates a Regular Expression into a DFA implementation
    *
    * @param r The regular Expression
    * @return DFA of r
    */
  def translateRegex(r: String) = {
    logger.info("Translating regular expression \""+r+"\" to NFA")
    //###### Setup ######
    var stack: List[NFA] = List()
    var opStack: List[Char] = List()
    var nextId: Int = 0
    var inputSet:Set[Char] = Set()
    var previous:Char = backspace
    var previousInput:Char = backspace
    /** changes previous symbol stores according to input type */
    def pushprev(c: Char):Unit = {
      if(isInput(c)) previousInput = c
      previous = c
    }
    def input(c:Char) = c match {
          case '\u0000' => "epsilon"
          case '\u0008' => "backspace"
          case x        => x
        }

    //###### Preprocessing ######
    /** Expands brace expressions into union versions */
    def braceExpand(s: String): List[Char] = {
      var previous = backspace
      /** creates a union operator between each character in the string */
      def createUnions(s: List[Char]):List[Char] = {
        if(s.isEmpty) List()
        else if(s.length == 1) List(s.head)
        else{
        if (s.head == '\\' && s.tail.head == '-'){
          previous = '\\'
          '-' :: '|' :: createUnions(s.tail)
        }
        else if(s.head == '-' && previous != '\\') {
          val n = s.tail.head
          logger.trace("creating union range between '" + previous + "' and '" + n + '\'')
          createUnions(asciirun(previous,n) ++ s.tail.tail)
        }
        else{
        previous = s.head
        logger.trace("adding union after '" + previous + '\'')
        s.head :: '|' :: createUnions(s.tail)
        }
      }
      }
      /** converts a character range into it's list representation */
      def asciirun(start: Char, end: Char):List[Char] = (start to end).toList
      if(s.isEmpty) List()
      else{
        if(s.head == '[')
        {
          val t = s.tail.takeWhile(c => c != ']').toList
          logger.trace("expanding brace expression \"" + t + '"')
          '('::createUnions(t) ++ (')' :: braceExpand(s.tail.dropWhile(c => c != ']').tail))
        }
        else{
          s.head :: braceExpand(s.tail)
        }
      }
    }

    /**
     * edits the input string to add concatenation
     * 
     * @param s the input string
     * @return input string with concatenations inserted
     */
    def concatExpand(s: List[Char]): List[Char] = {
      val o:List[Char] = List('*','+',')')
      var escaped:Boolean = false
      /** checks for brackets or other operators */
      def checkchars(s: List[Char]):List[Char] = {
        if(s.length > 1){
          val c1 = s.head
          val c2 = s.tail.head
          val xs = s.tail.tail
          val b1 = isInput(c1) || escaped || o.contains(c1)
          val b2 = isInput(c2) || c2 == '\\' || c2 == '('
          if(b1 && b2) { 
           logger.trace("adding concatenation between '" + c1 + "' and '" + c2 + '\'');
           escaped = c1 == '\\'
           c1 :: backspace :: checkchars(c2 :: xs)}
          else {escaped = c1 == '\\';c1 :: checkchars(s.tail)}
        }
        else s
      }
      checkchars(s.toList)
    }

    //###### Thompson construction algorithm ######
    /**
      * Translates the input string into a NFA
      *
      * @param s input string
      * @return (NFA of s or null,success value)
      */
    def translateToNFA(s: List[Char]): (NFA, Boolean) = {
      /**translates a single character into a NFA using the shunting yard algorithm and adds it to the stack.*/
      def translateAction(c: Char): Boolean = {
        logger.debug("translating '" + input(c) + '\'')
        //TODO: fix bracketing
        /** handles parentheses translation */
        def parenth: Boolean = {
          logger.trace("parenth")
          if(opStack.isEmpty) throw new RegexError("mismatched brackets",r)
          else{
            while(opStack.head != '('){
              if(!eval) false
            }
            opStack = opStack.tail
            true
          }
        }

        if (isInput(c)) {
          push(c); pushprev(c); true
        } else if(previous == '\\'){
          logger.trace("escaped operator '" + input(c) + '\'')
          push(c); pushprev(c); true
        } else if (c == '(') {
          logger.trace("adding ( to stack")
          opStack = c :: opStack; pushprev(c); true
        } else if (c == ')') parenth
        else if (opStack.isEmpty) {
          logger.trace("insert operator '"+input(c)+'\'')
          opStack = c :: opStack; pushprev(c);  true
        }
        else {
          if (!isOperator(c)) false
          while (!opStack.isEmpty && precedence(c, opStack.head)) {
            if (!eval) false
          }
          opStack = c :: opStack
          if (stack isEmpty) false
        }
        pushprev(c)
        true
      }
      if (s.isEmpty) {
        //eval remaining operators
        if ((for (op <- opStack) yield eval).exists(x => x == false)) false
        if(stack.isEmpty) throw new RegexError("Translation ended with empty stack",r)
        val fsa = stack.head
        //add the final state as an accepting state
        logger.debug("accepting NFA state: " + fsa.finalState)
        fsa.finalState.accepting = true
        fsa.addAccepting(fsa.finalState)
        (fsa, true)
      } else {
        val t = translateAction(s.head)
        if (!t) (null, false)
        else translateToNFA(s.tail)
      }
    }
    //###### Stack operations ######
    /**
      * adds a character to the stack in the form of a two state NFA
      * NFA is constructed as follows:
      * state 0 (c) -> state 1
      *
      * @param c character to add
      */
    def push(c: Char): Unit = {
      logger.trace("Push '"+input(c)+'\'')
      inputSet = inputSet.union(Set(c))
      val s0 = new NFAState(nextId)
      val s1 = new NFAState(nextId + 1)
      nextId = nextId + 2
      s0.addTransition(c, s1)
      stack = (new NFA(List(s1, s0))) :: stack
    }
    /**
      * retrieves an NFA from the stack with a success value
      *
      * @return (NFA or null,success value)
      */
    def pop: (NFA, Boolean) = {
      logger.trace("Pop")
      if (stack isEmpty) (null, false)
      else {
        val p = stack.head
        stack = stack.tail
        (p, true)
      }
    }


    //###### operator processing ######

    /** takes an operator from the stack and calls the appropriate method. 
     * @return success value from chosen operation
     */
    def eval: Boolean = {
      //evaluating operators and executing the correct action
      if (opStack isEmpty) false
      else {
        val o = opStack.head
        logger.trace("eval '"+input(o)+'\'')
        opStack = opStack.tail
        o match {
          case '*'      => star
          case '|'      => union
          case '+'      => plus
          case '\u0008' => concat
          case '\\'     => true
          case _        => false
        }
      }
    }

    /**
      * translates concatenation of characters using Thompson construction
      * and adds the result to the stack. 
      * Requires at least two NFAs on the stack.
      *
      * @return success value
      */
    def concat: Boolean = {
      logger.trace("concat")
      val (b, t1) = pop
      val (a, t2) = pop
      if (!t1 || !t2) false
      else{
        //add epsilon transition from the final state of A to the initial state of B
        //println("FSA A: initial state: " + a.initialState + ", final state: " + a.finalState)
        //println("FSA B: initial state: " + b.initialState + ", final state: " + b.finalState)
        a.finalState.addTransition(epsilon, b.initialState)
        val f = new NFA(a.states)
        f.addStates(b.states)
      stack = f :: stack
      true
      }
    }

    /**
      * translates Kleene star (*) into an NFA using Thompson construction, 
      * adds the result to the stack. 
      * Requires at least one NFA on the stack.
      *
      * @return success value
      */
    def star: Boolean = {
      //TODO: Work out why * doesn't accept no input
      logger.trace("star")
      //pop one result off the stack
      val (a, t) = pop
      if (!t) false
      else {
        val s0 = new NFAState(nextId)
        val s1 = new NFAState(nextId + 1)
        val fst = a.initialState
        val lst = a.finalState
        nextId = nextId + 2

        //create transition from s0 to s1
        s0 addTransition (epsilon, s1)
        //create transition from s0 to the initial state of A
        s0 addTransition (epsilon, fst)
        lst addTransition (epsilon, s1)
        lst addTransition (epsilon, fst)
        val s = new NFA(List(s0))
        s.addStates(a.getStates)
        s.addState(s1)
        stack = s :: stack
        true
      }
    }

    def plus: Boolean = {
      if(previous == backspace) false
      else
      push(previousInput)
      star
      concat
      true
    }
    /**
      * translates union (|) into an NFA using Thompson construction 
      * and adds the result to the stack. 
      * Requires at least two NFAs on the stack.
      *
      * @return success value
      */
    def union: Boolean = {
      logger.trace("union")
      //pop two sub-results A and B
      val (b, t1) = pop
      val (a, t2) = pop
      if (!t1 || !t2) false
      else {
        val s0 = new NFAState(nextId)
        val s1 = new NFAState(nextId + 1)
        val fstA = a.initialState
        val fstB = b.initialState
        val lstA = a.finalState
        val lstB = b.finalState
        nextId = nextId + 2

        //create epsilon transition from s0 to the initial states of A and B
        s0 addTransition (epsilon, fstA)
        s0 addTransition (epsilon, fstB)
        //create epsilon transition from the final states of A and B to s1
        lstA addTransition (epsilon, s1)
        lstB addTransition (epsilon, s1)

        //create new FSAs with s1 and s0
        val newB = new NFA(b.getStates)
        newB.addState(s1)
        val newA = new NFA(List(s0))
        newA.addStates(a.getStates)
        //add to the result set
        val s = new NFA (newA.getStates)
        s.addStates(newB.getStates)
        stack = s :: stack
        true
      }
    }

    //###### Subset construction algorithm ######
    /**
      * takes the epsilon closure of a set of NFAStates.
      *
      * @param t Set of NFAStates
      * @return Set of reachable NFAStates
      */
    def epsilonClosure(t: Set[NFAState]): Set[NFAState] = {
      logger.trace("epsilon closure of "+t)
      var result = t.toList
      var unprocessed = t.toList
      while(!(unprocessed isEmpty)){
        val fst = unprocessed.head
        unprocessed = unprocessed tail
        val epsilons = fst transition(epsilon)
        for {u <- epsilons if !result.contains(u)} yield {result = u::result; unprocessed = u::unprocessed}
      }
      logger.trace(result.toString)
      result.toSet
    }

    /**
      * uses the subset construction algorithm to create a DFA from the initial state of an NFA.
      *
      * @param s initial state of NFA
      * @return DFA of NFA s
      */
    def dTranslate(s: NFAState): DFA = {
      var dfaStates: List[DFAState] = List()
      nextId = 0
      logger.debug("Translating NFA to DFA")
      //starting state of DFA is epsilon closure of first state of NFA
      val dfaStartState = new DFAState(epsilonClosure(Set(s)),nextId)
      var unmarked = List(dfaStartState)
      var processing:DFAState = dfaStartState
      var result:List[DFAState] = List(dfaStartState)
      while (!(unmarked isEmpty)){
        processing = unmarked.head
        logger.trace("processing "+processing)
        unmarked = unmarked.tail
        for{c <- inputSet
            //if processing.transitions.contains(c)
            } yield {
              logger.trace("processing epsilon closure of "+processing+" on '"+c + '\'')
              val move = processing nfaMove c
              if(!move.isEmpty){
              val closure = epsilonClosure(move)
              if(!(result exists(x => x.nfaStates == closure)))
              {
                nextId += 1
                val state = new DFAState(closure,nextId)
                logger.trace("adding " + state + " to result")
                processing.addTransition(c,state)
                result = state :: result
                unmarked = state :: unmarked
              }
              else{
                logger.trace("Subset already exists")
                val res = result.find(x => x.nfaStates == closure) match {
                  case None => throw new RegexError("could not find matched DFAState for epsilon closure",r)
                  case Some(value) => value
                }
                processing.addTransition(c,res)
              }
            }
            else{logger.trace("Transition not found")}
          }
      }
      new DFA(result)
    }

    //###### DFA Optimisation ######
    //TODO:Debug and verify correctness
    /**
      * removes redundant/dead end states from the DFA
      *
      * @param d DFA to reduce
      * @return optimised DFA
      */
    def dfaReduce(d:DFA):DFA = {
      /** checks if the State is a dead end */
      def deadEnd(s: DFAState):Boolean = {
       if(s.accepting) false
       else
       if(s.transitions.keySet.isEmpty) true
       else s.transitions.exists(p => !(p._2.diff(Set(s)).isEmpty))
      }
      for (state <- d.states if(deadEnd(state))) yield {
        d.states = d.states diff List(state)
        for (s <- d.states) yield {s.removeTransitions(state)}}
      d
    }
    //###### Execution ######
    //handling of empty regex
    if(r.isEmpty){
      val s = new NFAState(0)
      s.accepting = true
      val d = new DFAState(Set(s),0)
      val dfa = new DFA(List(d))
      dfa
    }
    else {
      val b = braceExpand(r)
      logger.trace("converted braces: \""+b+'"')
      val c = concatExpand(b)
      logger.trace("converted concatenations: \""+c+'"')
    if (!translateToNFA(c)._2)
      throw new RegexError("failed to parse regex",r)
    if (stack isEmpty) throw new RegexError("no NFA found",r)
    val nfa = stack.head
    if (!stack.tail.isEmpty)
      throw new RegexError(
        "unresolved states: " +
          (for {
            f <- stack.tail
            s <- f.getStates
          } yield s.id)
          ,r)
    val d = dTranslate(nfa initialState)
    logger.debug("included states: "+d.getStates)
    logger.whenTraceEnabled{for(s <- d.getStates) yield {logger.trace(s"State: " + s + ", accepting: " + s.accepting)}}
    d
    }
  }
}

class RegexError(msg: String,input: String) extends Error("Failed to translate Regular Expression: " + msg + ", input:\"" + input + '"') with LazyLogging{
  logger.error("Failed to translate Regular Expression: " + msg + ", input:\"" + input + '"')
}