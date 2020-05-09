package scalaLex
import scala.util.Try
import scala.collection.immutable.Nil
import scala.language.postfixOps
import org.slf4j.LoggerFactory
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.core.util.StatusPrinter
import com.typesafe.scalalogging.LazyLogging
import scala.annotation.tailrec

/**Thompson construction and subset construction implemented using shunting yard algorithm to produce a DFA from a given regular expression string
 */
object regexParser extends LazyLogging {

  //###### Logging ######
  //val logger = Logger(LoggerFactory.getLogger(expressions.getClass()))
  // assume SLF4J is bound to logback in the current environment
  // private val lc:LoggerContext = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
  // print logback's internal status
  // StatusPrinter.print(lc)

  //###### Character evaluation ######
  /** unicode for a backspace character - used to represent concatenation*/
  private val backspace: Char = '\u0008'
  private val empty:Char = '\u0000'
  /** the characters that must be escaped in the input string*/
  private val illegal: Set[Char] = Set(backspace)
  /** chars that represent regex operators */
  private val operators: Set[Char] = Set('?','"','-','^','|', '*', '+', backspace,'(',')','\\','[',']','{','}','/','$')
  /** checks if a character is an input or not */
  private def isInput(c: Char) = !isOperator(c)
  /** checks if a character is in the set of operators or not */
  private def isOperator(c: Char) = operators.contains(c)
  
  private var prevOp = empty
  /**
    * Translates a Regular Expression into a DFA implementation
    *
    * @param r The regular Expression
    * @return DFA of r
    */
  def translateRegex(r: String) = {
    logger.info("Translating regular expression \""+r+"\" to NFA")
    /** the id of the next state to be created */
    var nextId: Int = 0
    /** the set of chars with transitions available - used with subset construction */
    var inputSet:List[(Set[Char],Boolean)] = Nil
    /** gets the next state id to be used, ensures that all created states have a unique id. */
    def getId:Int = {val i = nextId; nextId += 1; i}
    /** converts an input char into a readable/printable format */
    def input(c:Char) = c match {
          case '\u0000' => "epsilon"
          case '\u0008' => "backspace"
          case x        => x
        }
    //###### Preprocessing ######
    /** converts the input string into RegexToken representations */
    def makeSymbols(s:List[Char]): List[RegexToken] = {
      /*
      0 = nothing
      1 = charset
      2 = quoted/escaped
      3 = quantifier
      */
      var escaped = false
      var inverted = false
      var op = 0
      var charset:List[Char] = Nil
      var quantOp:String = ""
      var fst = 0
      var snd = 0
      var prev:RegexToken = null
      @tailrec
      def makeSymbol(s: List[Char],a: List[RegexToken]): List[RegexToken] = s match {
        //return symbols
        case Nil => a
        //escape next character
        case '\\'::xs if !escaped && op < 3 =>
          logger.trace("escaping next operator")
          escaped = true
          makeSymbol(xs,a)
        //escaped operator
        case x::xs if escaped && op == 0 =>
          escaped = false
          val o = Operator(x,true)
          prev = o
          makeSymbol(xs,a:+o)
        case x::xs if escaped && op == 1 =>
          escaped = false
          charset = x::'\\':: charset
          makeSymbol(xs,a)
        case x::xs if escaped && op == 2 =>
          escaped = false
          charset = x::charset
          makeSymbol(xs,a)
        case x::xs if illegal.contains(x) && op == 0 =>
          val o = Operator(x,true)
          prev = o
          makeSymbol(xs,a:+o)
        //quotations
        case '"'::xs if !escaped && op == 0 =>
          charset = Nil
          op = 2
          makeSymbol(xs,a)
        case '"'::xs if !escaped && op == 2 =>
            op = 0
            val c = charset.reverse
            logger.trace("adding escaped quote sequence " + c)
            val o = QuoteSeq(c)
            prev = o
            charset = Nil
            makeSymbol(xs,a:+o)
        //char set
        case '['::'^'::xs if !escaped && op != 1 => 
          op = 1
          inverted = true
          makeSymbol(xs,a)
        case '['::xs if !escaped && op != 1 => 
          op = 1
          makeSymbol(xs,a)
        case ']'::xs if !escaped && op == 1 =>
          val c = charset.reverse
          logger.trace("Created Character set " + c)
          op = 0
          val o = CharSet(c,inverted)
          prev = o
          inverted = false
          charset = Nil
          makeSymbol(xs,a:+o)
        case x::xs if op == 1 || op == 2 =>
          charset = x ::charset
          makeSymbol(xs,a)
        //quantifiers
        case '{'::xs if !escaped && op == 0 =>
          op = 3
          quantOp = ""
          fst = 0
          snd = 0
          makeSymbol(xs,a)
        case '}'::xs if !escaped && op == 3 =>
          if(!quantOp.isEmpty) {snd = Integer.parseInt(quantOp)}
          val c = Quantifier(prev,fst,snd)
          prev = c
          quantOp = ""
          fst = 0
          snd = 0
          op = 0
          makeSymbol(xs,a:+c)
        case ','::xs if !escaped && op == 3 =>
          if(!quantOp.isEmpty) {fst = Integer.parseInt(quantOp)}
          quantOp = ""
          makeSymbol(xs,a)
        case x::xs if !escaped && op == 3 =>
          quantOp = quantOp + x
          makeSymbol(xs,a)
        //normal operator
        case x::xs if !escaped && op == 0 && isOperator(x) => 
          val o = Operator(x,false)
          prev = o
          makeSymbol(xs, a:+o)
        //normal input symbol
        case x:: xs if isInput(x) => 
          val o = Input(x)
          prev = o
          makeSymbol(xs,a:+o)
      }
      makeSymbol(s,Nil)
    }

    /** Creates BracketExpresssions within the input string */
    def makeExpression(s: List[RegexToken]):List[RegexToken] ={
      /** the expression within brackets */
      var inner:List[RegexToken] = Nil
      /** the number of unclosed bracket pairs */
      var bracketdepth = 0
      /** creates a new BracketExpression from the inner expression up to the next parentheses */
      def addExpression = {
        //pulls a bracketed expression off the input stack - similar to shunting yard algorithm
        @tailrec
        def getInner(l: List[RegexToken],a: List[RegexToken]):List[RegexToken] = l match {
          case Nil => throw new RegexError("mismatched brackets found",r)
          case Operator('(',false) :: xs => 
          inner = inner.tail
          a
          case x::xs => 
          inner = inner.tail
          getInner(xs,x::a)
        }
          val i = getInner(inner,Nil)
          val o = BracketExpression(i)
          bracketdepth -=1
          inner = o :: inner
      }
      @tailrec
      def findBrackets(s: List[RegexToken],a: List[RegexToken]):List[RegexToken] = s match {
        case Nil => a
        case x@Operator('(',false)::xs =>
          bracketdepth += 1
          inner = x.head :: inner
          findBrackets(xs,a)
        case Operator(')',false)::Quantifier(sym,fst,snd)::xs =>
          addExpression
          val i = inner.head
          val n = Quantifier(i,fst,snd)
          if(bracketdepth == 0)
          findBrackets(n::xs,a:+i)
          else
          findBrackets(n::xs,a)
        case x@Operator(')',false)::xs =>
          addExpression
          val i = inner.head
          if(bracketdepth == 0)
          findBrackets(xs,a:+i)
          else
          findBrackets(xs,a)
        case x::xs if bracketdepth > 0 =>
          inner = x::inner
          findBrackets(xs,a)
        case x::xs if bracketdepth <= 0 =>
          findBrackets(xs,a:+x)
      }
      findBrackets(s,Nil)
    }

    /** parses quantifiers in the input list */
    def quantExpand(s:List[RegexToken]):List[RegexToken] = {
      @tailrec
      def findQuant(s: List[RegexToken],a: List[RegexToken]):List[RegexToken] = {
        def makeQuant(s: RegexToken, f: Int, l: Int):List[RegexToken] = {
          var r:List[RegexToken] = Nil
          val q = Operator('?',false)
          for(i <- 1 until f) yield {
            r = s::r
          }
          for(i <- f until l) yield {
            r = q::s::r
          }
          r
        }
        s match {
          case Nil => a
          case Quantifier(s,f,l)::xs =>
            val r = makeQuant(s,f,l)
            findQuant(xs,a ++ r)
          case BracketExpression(s)::xs =>
            logger.trace("processing bracketed expression " + s)
            val o = BracketExpression(quantExpand(s))
            findQuant(xs,a:+o)
          case x::xs =>
            findQuant(xs,a:+x)
        }
      }
      findQuant(s,Nil)
    }

    /** Expands brace expressions into full character sets */
    def braceExpand(s: List[RegexToken]):List[RegexToken]= {
      var escaped = false
      @tailrec
      def findBraces(s: List[RegexToken],a: List[RegexToken]): List[RegexToken] = {
        @tailrec
        def createRange(s: List[Char],a: List[Char]): List[Char] = 
        {
          s match{
            //return symbols
            case Nil => a
            case '\\'::xs if !escaped =>
              escaped = true
              createRange(xs,a)
            case x::xs if escaped =>
              escaped = false
              createRange(xs,a:+x)
            //convert character range e.g. [a-z]
            case x0::'-'::x1::xs if !escaped =>
              logger.trace("creating character range from " + x0 + " to " + x1)
              val b = Math.min(x0,x1).toChar
              val e = Math.max(x0,x1).toChar
              val range = b to e
              //logger.trace("a0: " + a0)
              createRange(x1::xs,a ++ range.toList)
            //continue
            case x::xs =>
              createRange(xs,a:+x)
          }
        }
        s match {
          //return symbols
          case Nil => a
          //finds char set
          case CharSet(s,i) :: xs =>
            logger.trace("processing character set " + s)
            val c = CharSet(createRange(s,Nil),i)
            logger.trace("adding sequence " + c)
            findBraces(xs,a:+c)
          case BracketExpression(s)::xs =>
            logger.trace("processing bracketed expression (" + s + ')')
            val o = BracketExpression(braceExpand(s))
            findBraces(xs,a:+o)
          //continue
          case x :: xs =>
            //logger.trace("skipping character " + input(x.symbol))
            findBraces(xs,a:+x)
        }
      }
      findBraces(s,Nil)
    }

    /** edits the input string to add concatenation operators ('u\0008', or backpace character)*/
    def concatExpand(s: List[RegexToken]):List[RegexToken]= {
      val o:List[Char] = List('*','+',')','?')
      @tailrec
      def checkchars(s: List[RegexToken],a: List[RegexToken]):List[RegexToken] = {
        /** checks the first symbol to add a concatenation */
        def checkfirst(c: RegexToken):Boolean = c match {
          //input
          case Input(x) => true
          case CharSet(s,i) => true
          case Quantifier(sym,fst,snd) => true
          case BracketExpression(x) => true
          //one of o operators
          case Operator(x,e) if o.contains(x)||e => true
          //else false
          case x => false
        }
        /** checks the next symbol to add a concatenation */
        def checknext(c: RegexToken):Boolean = c match {
          //start of parentheses
          case BracketExpression(x) => true
          case CharSet(s,i) => true
          case Quantifier(sym,fst,snd) => true
          //escaped operator
          case Operator(x, true) => true
          //input
          case Input(x) => true
          //otherwise false
          case x => false
        }
        s match {
          //return symbols
          case Nil => a
          //eval last expression
          case BracketExpression(s)::Nil =>
            val o = BracketExpression(concatExpand(s))
            checkchars(Nil,a:+o)
          //ignore single symbol
          case x :: Nil => checkchars(Nil,a:+x)
          //eval inner expression
          case BracketExpression(s)::c2::xs =>
            logger.trace("processing bracketed expression " + s)
            val o = BracketExpression(concatExpand(s))
            var ls = a:+o
            if(checknext(c2)) {ls = ls :+ Operator(backspace,false)}
            checkchars(c2::xs,ls)
          //add concatenation
          case c1 :: c2 :: xs if(checkfirst(c1)&&checknext(c2)) => 
            logger.trace("adding concatenation between '" + c1.symbol + "' and '" + c2.symbol + '\'');
            checkchars(c2::xs,a:+c1:+Operator(backspace,false))
          //do not add concatenation
          case c1 :: c2 :: xs if !(checkfirst(c1)&&checknext(c2)) =>
            checkchars(c2::xs,a:+c1)
        }
      }
      checkchars(s,Nil)
    }


    //###### Thompson construction algorithm ######
    /**
      * Translates the input string into a NFA using Thompson Construction
      *
      * @param s input string
      * @return (NFA of s or null,success value)
      */
    def translateToNFA(sym: List[RegexToken]): (NFA,Boolean) = {
      //###### Setup ######
      /** stores working list of NFAs - used with shunting yard algorithm */
      var stack: List[NFA] = Nil
      /** stores unprocessed operators */
      var opStack: List[Char] = Nil
      val badChars = Set('[',']','{','}')
      var startMatching = false
      var endMatching = false
      /** enforces operator precedence on the stack */
      def precedence(l: Char, r: Char) = {
        val o = List('|','*','+','?')
        logger.trace("precedence '" + l + "' '" + r + '\'')
        if (l == r) true
        else if (o.contains(l)) false
        else if (o.contains(r)) true
        else if (l == '\\') false
        else if (l == backspace) false
        else if (r == backspace) true
        else true
      }
      @tailrec
      def translateSymbols(s: List[RegexToken]): NFA = {
        /**translates a single character into a NFA using the shunting yard algorithm and adds it to the stack.*/
        def translateSymbol(c: RegexToken): Boolean = {
          c match {
            //input
            case Input(x) => 
              push(x); true
            //escaped operator
            case Operator(x,true) =>
              logger.trace("escaped operator '" + input(x) + '\'')
              push(x); true
            case QuoteSeq(s) => 
              logger.trace("processing quotation set " + s)
              push(s.head)
          for (i <- s.tail){push(i);opStack = backspace :: opStack};true
            case CharSet(s, inverted) => 
              logger.trace("Processing char set " + s)
              pushAll(s,inverted); true
            //parentheses
            case BracketExpression(s) =>
             val n = translateToNFA(s)
             stack = n._1 ::stack;true
            //char sets
            case Operator(x,false) if badChars.contains(x) => throw new RegexError("Failed to process symbol: " + x,r)
            //empty operator stack
            case Operator(x, false) if opStack isEmpty => 
              logger.trace("insert operator '"+input(x)+'\'')
              opStack = x :: opStack; true
            //otherwise
            case Operator(x,false) => 
              while (!opStack.isEmpty && precedence(x, opStack.head)) {
                if (!eval) throw new RegexError("Failed to evaluate operator",r)
              }
              opStack = x :: opStack
              if (stack isEmpty) throw new RegexError("Empty stack after operator processing",r)
              else {
                true
              }
          }
        }
        s match {
          //empty input
          case Nil => 
            if ((for (op <- opStack) yield eval).exists(x => x == false)) throw new RegexError("Symbol was processed incorrectly: " + prevOp,r)
            if(stack.isEmpty) throw new RegexError("Translation ended with empty stack",r)
            val s = stack.head
            stack = stack.tail
            s
          case x::xs =>
            val t = translateSymbol(x)
            if(!t) throw new RegexError("failed to parse symbol " + x,r)
            translateSymbols(xs)
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
        //add c to inputSet
        inputSet = (Set(c),false)::inputSet
        //create new NFA(s0(c) -> s1)
        val s0 = new NFAState(getId)
        val s1 = new NFAState(getId)
        s0.addTransition(false, s1,c)
        stack = (new NFA(List(s1, s0))) :: stack
      }

      def pushAll(l: List[Char],i: Boolean):Unit = {
        logger.trace("PushAll")
        inputSet = (l.toSet,i)::inputSet
        val s0 = new NFAState(getId)
        val s1 = new NFAState(getId)
        if(l.isEmpty){s0.addEmptyTransition(i,s1)}
        else{
        s0.addTransition(i,s1,l:_*)
        }
        stack = (new NFA(List(s1,s0))) :: stack
      }

      /**
        * retrieves an NFA from the stack with a success value
        *
        * @return (NFA or null,success value)
        */
      def pop: (NFA, Boolean) = {
        logger.trace("Pop")
        if (stack isEmpty) throw new RegexError("popped from empty stack",r)
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
        if (opStack isEmpty) throw new RegexError("Tried to evaluate empty operator stack",r)
        else {
          val o = opStack.head
          prevOp = o
          logger.trace("eval '"+input(o)+'\'')
          opStack = opStack.tail
          o match {
            case '*'      => star
            case '|'      => union
            case '+'      => plus
            case '?'      => lazyOp
            case '\u0008' => concat
            case '/'      => concat
            case '^'      => startMatch
            case '$'      => endMatch
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
        if (!t1 || !t2) throw new RegexError("failed to process concatenation",r)
        else{
          //add epsilon transition from the final state of A to the initial state of B
          a.finalState.epsilons_(b.initialState)
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
        if (!t) throw new RegexError("Failed to process * operator",r)
        else {
          val s0 = new NFAState(getId)
          val s1 = new NFAState(getId)
          val fst = a.initialState
          val lst = a.finalState
          //create transitions s0 -> s1, s0 -> fst, lst - s1, lst -> fst
          s0 epsilons_(s1)
          s0 epsilons_(fst)
          lst epsilons_(s1)
          lst epsilons_(fst)
          //populate NFA
          val s = new NFA(List(s0))
          s.addStates(a.states)
          s.addState(s1)
          //add to stack
          stack = s :: stack
          true
        }
      }

      def lazyOp: Boolean = {
        logger.trace("lazyOp ?")
        val (a,t1) = pop
        if(!t1) throw new RegexError("Incorrect use of ?",r)
        else{
        //create new initial and final state
        val s0 = new NFAState(getId)
        val s1 = new NFAState(getId)
        val n = new NFA(List(s0))
        //add transitions s0 -> initial, s0 -> s1, final -> s1
        a.finalState.epsilons_(s1)
        s0.epsilons_(a.initialState)
        s0.epsilons_(s1)
        //populate NFA
        n.addStates(a.states)
        n.addState(s1)
        //add to stack
        stack = n :: stack
        true
        }
      }

      def plus: Boolean = {
        //push extra input symbol
        val (a,b) = pop
        if (!b) throw new RegexError("Failed to process + quantifier",r)
        //create new initial and final states s0 and s1
        val s0 = new NFAState(getId)
        val s1 = new NFAState(getId)
        val n = new NFA(List(s0))
        //add transitions s0 -> initial, final -> s1, s1 -> s0
        s0.epsilons_(a.initialState)
        a.finalState.epsilons_(s1)
        a.finalState.epsilons_(a.initialState)
        //populate NFA
        n.addStates(a.states)
        n.addState(s1)
        //add to stack
        stack = n :: stack
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
          val s0 = new NFAState(getId)
          val s1 = new NFAState(getId)
          val fstA = a.initialState
          val fstB = b.initialState
          val lstA = a.finalState
          val lstB = b.finalState

          //create epsilon transition from s0 to the initial states of A and B
          s0 epsilons_(fstA)
          s0 epsilons_(fstB)
          //create epsilon transition from the final states of A and B to s1
          lstA epsilons_(s1)
          lstB epsilons_(s1)

          //create new FSAs with s1 and s0
          val newB = new NFA(b.states)
          newB.addState(s1)
          val newA = new NFA(List(s0))
          newA.addStates(a.states)
          //add to the result set
          val s = new NFA (newA.states)
          s.addStates(newB.states)
          stack = s :: stack
          true
        }
      }
      //TODO: Implement evaluation using start matching and end matching
      def startMatch:Boolean = {
        logger.debug("Start matching")
        startMatching = true
        true
      }
      def endMatch:Boolean = {
        logger.debug("End matching")
        endMatching = true
        true
      }
    //add the final state as an accepting state
    val nfa = translateSymbols(sym)
    logger.debug("accepting NFA state: " + nfa.finalState)
    nfa.startMatch = startMatching
    nfa.endMatch = endMatching
    nfa.finalState.accepting = true
    nfa.addAccepting(nfa.finalState)
    if (!stack.isEmpty) throw new RegexError(
        "unresolved states: " +
          (for {
            f <- stack
            s <- f.states
          } yield s.id)
          ,r)
      (nfa,true)
    }

    //###### Subset construction algorithm ######
    /** takes the epsilon closure of a set of NFAStates */
    def epsilonClosure(t: Set[NFAState]): Set[NFAState] = {
      //initialise with input set
      logger.trace("epsilon closure of "+t)
      var result = t.toList
      var unprocessed = t.toList
      //get epsilon transitions of states
      while(!(unprocessed isEmpty)){
        val fst = unprocessed.head
        unprocessed = unprocessed tail
        val epsilons = fst.epsilons
        //add results to queue to be processed
        for {u <- epsilons if !result.contains(u)} yield {result = u::result; unprocessed = u::unprocessed}
      }
      logger.trace(result.toString)
      result.toSet
    }

    /** uses the subset construction algorithm to create a DFA from the initial state of an NFA */
    def dTranslate(s: NFAState): DFA = {
      var dfaStates: List[DFAState] = List()
      nextId = 0
      logger.debug("Translating NFA to DFA")
      logger.trace("input set: " + inputSet)
      //starting state of DFA is epsilon closure of first state of NFA
      val dfaStartState = new DFAState(epsilonClosure(Set(s)),getId)
      var unmarked = List(dfaStartState)
      var processing:DFAState = dfaStartState
      var result:List[DFAState] = List(dfaStartState)
      while (!(unmarked isEmpty)){
        //pop a state from the stack
        processing = unmarked.head
        logger.trace("processing "+processing)
        unmarked = unmarked.tail
        //for all transition symbols in input set
        for{(s,i) <- inputSet
            } yield {
              //special case for [^] or []
              if(s.isEmpty){
                val move = processing.emptyNFAMove
                if(!move.isEmpty){
                  val closure = epsilonClosure(move)
              //check if this DFAState has already been processed
              if(!(result exists(x => x.nfaStates == closure)))
              {
                //create new DFAState and add it to result set
                val state = new DFAState(closure,getId)
                logger.trace("adding " + state + " to result")
                processing.addTransition(i,state)
                result = state :: result
                unmarked = state :: unmarked
              }
              else{
                //add transition to existing DFAState
                logger.trace("Subset already exists")
                val res = result.find(x => x.nfaStates == closure) match {
                  case None => throw new RegexError("could not find matched DFAState for epsilon closure",r)
                  case Some(value) => value
                }
                processing.addTransition(i,res)
              }
            }
            else{logger.trace("Transition not found")}
          }
          else{
              //iterate through characters
              for (c <- s) yield {
              logger.trace("processing epsilon closure of "+processing+" on '"+c + '\'')
              val move = processing nfaMove(c,i)
              if(!move.isEmpty){
              val closure = epsilonClosure(move)
              //check if this DFAState has already been processed
              if(!(result exists(x => x.nfaStates == closure)))
              {
                //create new DFAState and add it to result set
                val state = new DFAState(closure,getId)
                logger.trace("adding " + state + " to result")
                processing.addTransition(i,state,c)
                result = state :: result
                unmarked = state :: unmarked
              }
              else{
                //add transition to existing DFAState
                logger.trace("Subset already exists")
                val res = result.find(x => x.nfaStates == closure) match {
                  case None => throw new RegexError("could not find matched DFAState for epsilon closure",r)
                  case Some(value) => value
                }
                processing.addTransition(i,res,c)
              }
            }
            else{logger.trace("Transition not found")}
            }
          }
        }
      }
      new DFA(result,r)
    }

    //###### DFA Optimisation ######
    /**
      * removes redundant/dead end states from the DFA
      *
      * @param d DFA to reduce
      * @return optimised DFA
      */
    def dfaReduce(d:DFA):DFA = {
      for (state <- d.states if(state.deadEnd)) yield {
        logger.trace("Removing " + state);
        d.removeState(state)
        for (s <- d.states) yield {s.removeTransitions(state)}}
      d
    }
    //###### Execution ######
    //handling of empty regex
    if(r.isEmpty){
      val s = new NFAState(0)
      s.accepting = true
      val d = new DFAState(Set(s),0)
      val dfa = new DFA(List(d),r)
      dfa
    }
    else {
      //preprocessing
      logger.debug("preprocessing symbols")
      val s = makeSymbols(r.toList)
      logger.debug("symbol list: " + s)
      logger.debug("processing bracketed expressions")
      val br = makeExpression(s)
      logger.debug("processed bracket expressions: " + br)
      //val quant = quantExpand(br)
      logger.debug("processing character sets")
      val ch = braceExpand(br)
      logger.debug("converted char sets: "+ch)
      logger.debug("processing quantifiers")
      val quant = quantExpand(ch)
      logger.debug("Processed quantifiers: " + quant)
      logger.debug("adding concatenations")
      val c = concatExpand(quant)
      logger.debug("converted concatenations: "+c)
      //Thompson construction
      val (nfa,t) = translateToNFA(c)
      if(!t) {throw new RegexError("failed to parse regex",r)}
      //subset construction
      val d = dTranslate(nfa initialState)
      logger.debug("included states: "+d.states)
      logger.whenTraceEnabled{for(s <- d.states) yield {logger.trace(s"State: " + s + ", accepting: " + s.accepting)}}
      dfaReduce(d)
    }
  }
}

/** A tokenised component of the Regex input */
private abstract class RegexToken(val symbol: Char) {
  override def equals(x: Any): Boolean = {
    if (!x.isInstanceOf[RegexToken]) false
    else{
      val s = x.asInstanceOf[RegexToken]
      this.symbol == s.symbol
    }
  }
  override def toString(): String = symbol.toString
}

/** encodes a quantifier {}
 * @param sym the operand for this quantifier
 * @param min the minimum number of matches
 * @param max the maximum number of matches 
 */
private case class Quantifier(sym: RegexToken,min: Int,max: Int) extends RegexToken('{'){
  override def toString(): String = "{" + min + ',' + max + '}'
}

/** encodes an escaped sequence of input symbols "" */
private case class QuoteSeq(s: List[Char]) extends RegexToken('"'){
  override def toString(): String = {
    val sb = StringBuilder.newBuilder
    sb.append('"')
    s.map(s => sb.append(s))
    sb.append('"')
    sb.mkString
  }
}

/** encodes a character set [] 
 * @param s the characters to encode for
 * @param inverted indicates whether this char set was inverted using a ^ operator at the start
*/
private case class CharSet(s: List[Char],inverted: Boolean) extends RegexToken('[') {
  override def toString(): String = {
    val i = inverted match {
      case true =>
        "^"
      case false =>
        ""
    }
    val sb = StringBuilder.newBuilder
    sb.append('[')
    sb.append(i)
    s.map(s => sb.append(s))
    sb.append(']')
    sb.mkString
  }
}

/** encodes a bracketed expression to be processed internally () */
private case class BracketExpression(s: List[RegexToken]) extends RegexToken('(') {
  override def toString(): String = {
    val sb = StringBuilder.newBuilder
    sb.append('(')
    s.map(s => sb.append(s))
    sb.append(')')
    sb.mkString
  }
}

/** encodes an operator 
 * 
 * @param symbol the operand for this operator
 * @param op the symbol encoding this operator
 * @param escaped indicates if this operator was escaped using a \
*/
private case class Operator(op: Char,escaped: Boolean) extends RegexToken(op) {
  override def equals(x: Any): Boolean = {
    if (!x.isInstanceOf[Operator]) false
    else {
      val s = x.asInstanceOf[Operator]
      this.op == s.op && this.escaped == s.escaped
    }
  }
  override def toString(): String = if(escaped) "\\"+op else op.toString
}

/** A regex input symbol */
private case class Input(s: Char) extends RegexToken(s)

/**
  * An error class indicating that the translation of a Regular Expression has failed
  *
  * @param msg reason for failure
  * @param input regex input symbols
  */
class RegexError(msg: String,input: String) extends Error("Failed to translate Regular Expression: " + msg + ", input:\"" + input + '"') with LazyLogging{
  logger.error("Failed to translate Regular Expression: " + msg + ", input:\"" + input + '"')
}