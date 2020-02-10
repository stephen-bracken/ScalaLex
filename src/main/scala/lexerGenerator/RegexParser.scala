package lexerGenerator
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
  /** unicode for an empty character - used to represent an epsilon transition*/
  private val epsilon: Char = '\u0000'
  /** unicode for a backspace character - used to represent concatenation*/
  private val backspace: Char = '\u0008'
  /** the characters that are not allowed in the input string*/
  private val illegal: Set[Char] = Set(epsilon, backspace)
  /** chars that represent regex operators */
  private val operators: Set[Char] = Set('-','^','|', '*', '+', epsilon, backspace,'(',')','\\','[',']')

  private val allChars:Set[RegexToken] = {
    val chars = Char.MinValue to Char.MaxValue
    (for(c <- chars) yield (new Input(c,true))).toSet
  }
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
    /** stores working list of NFAs - used with shunting yard algorithm */
    var stack: List[NFA] = Nil
    /** stores unprocessed operators */
    var opStack: List[Char] = Nil
    /** the id of the next state to be created */
    var nextId: Int = 0
    /** the set of chars with transitions available - used with subset construction */
    var inputSet:Set[Char] = Set.empty

    /** indicates whether the current character was escaped */
    var escaped:Boolean = false
    /** the previous symbol including operators - used with backslash (\) */
    var previous:Char = backspace
    /** the previous input symbol (excluding operators) - used with plus (+) */
    var previousInput:Char = backspace
    /** changes previous symbol stores according to input type */
    var braceset:Set[Char] = Set.empty
    var inBrace = false
    def pushprev(c: Char):Unit = {
      if(isInput(c)) previousInput = c
      previous = c
    }

    /** converts an input char into a readable/printable format */
    def input(c:Char) = c match {
          case '\u0000' => "epsilon"
          case '\u0008' => "backspace"
          case x        => x
        }

    //###### Preprocessing ######
    @tailrec
    def makeSymbols(s: List[Char],a: List[RegexToken]): List[RegexToken] = s match {
      case Nil => a.reverse
      case '\\'::xs if !escaped =>
        escaped = true
        makeSymbols(xs,a)
      case '['::xs if !escaped && !inBrace => 
        inBrace = true
        makeSymbols(xs,new Operator('[',false,false)::a)
      case ']'::xs if !escaped && inBrace =>
        inBrace = false
        makeSymbols(xs,new Operator(']',false,false)::a)
      case x::xs if escaped =>
        escaped = false
        makeSymbols(xs,new Operator(x,true,inBrace)::a)
      case x::xs if !escaped && isOperator(x) => 
        makeSymbols(xs, new Operator(x,false,inBrace)::a)
      case x:: xs if isInput(x) => 
        makeSymbols(xs,new Input(x,inBrace)::a)
    }


    def braceExpand(s: List[RegexToken]):List[RegexToken]= {
      var invertedBrace = false
      var braceSymbols:List[RegexToken] = Nil
      logger.debug("processing character sets")
      @tailrec
      def findBraces(s: List[RegexToken],a: List[RegexToken]): List[RegexToken] = {
        @tailrec
        def createRange(s: List[RegexToken],a: List[RegexToken]): List[RegexToken] = 
        {
          s match{
            case Nil => a.reverse
            case x0::Operator('-',false,true)::x1::xs if x0.inRange && x1.inRange =>
              logger.trace("creating character range from " + x0.symbol + " to " + x1.symbol)
              val b = Math.min(x0.symbol,x1.symbol)
              val e = Math.max(x0.symbol,x1.symbol)
              val range = b to e
              var a0:Seq[RegexToken] = a
              for (i <- range) yield{a0 = new Input(i.toChar,true)+:a0}
              //logger.trace("a0: " + a0)
              createRange(xs,a0.toList)
            case x::xs if !x.inRange =>
              throw new RegexError("Unexpected symbol in range: " + x,r)
            case x::xs if x.inRange =>
              createRange(xs,x::a)
          }
        }
        def invertChars(s:List[RegexToken]): List[RegexToken] = {
          var charSet:Set[RegexToken] = allChars
          for(c <- s) yield(charSet = charSet.diff(Set(c)))
          charSet.toList
        }
        s match {
          case Nil => a.reverse
          case x :: Nil => 
            if(x.inRange) {
              braceSymbols = x :: braceSymbols
              logger.trace("processing range expression " + braceSymbols.reverse)
              val c = createRange(braceSymbols,Nil)
              logger.trace("adding sequence " + c)
              if(invertedBrace) findBraces(Nil,invertChars(c) ++ a)
              else findBraces(Nil,c++a)
            }
            else findBraces(Nil,x::a)
          case x@Operator('[',false,false)::Operator('^',false,true)::xs =>
            invertedBrace = true
            inBrace = true
            findBraces(xs,x.head::a)
          case x@Operator('[',false,false)::x1::xs if x1.inRange =>
            inBrace = true
            //braceSymbols = List(x1)
            findBraces(x1::xs,x.head::a)
          case x@ x0 :: Operator(']',false,false) :: xs if x0.inRange =>
            inBrace = false
            braceSymbols = x0 :: braceSymbols
            logger.trace("processing range expression " + braceSymbols.reverse)
            val c = createRange(braceSymbols,Nil)
            logger.trace("adding sequence " + c)
            if(invertedBrace) {
              logger.trace("inverting range")
              findBraces(x.tail,invertChars(c)++a)
            }
            else findBraces(x.tail,c ++ a)
          case x :: xs if x.inRange =>
            logger.trace("adding " + input(x.symbol) + " to range set")
            braceSymbols = x :: braceSymbols
            findBraces(xs,a)
          case x :: xs if !x.inRange =>
            logger.trace("skipping character " + input(x.symbol))
            findBraces(xs,x::a)
        }
      }
      findBraces(s,Nil)
    }
    /** Expands brace expressions into union versions */
    /*def braceExpand(s: List[Char]): List[Char] = {
      var previous = backspace
      var inverse = false
      /** creates a union operator between each character in the string */
      def createUnions(s: List[Char]):List[Char] = s match {
            case Nil =>  Nil
            //case x::Nil if(isOperator(x)) => List('\\',x)
            case '\\'::xs if !escaped => 
                //escaping of operators
                previous = '\\'
                logger.trace("escaping next symbol")
                escaped = true
                createUnions(xs)
            case '\\' :: '\\' :: xs if !escaped =>
                '\\' :: '\\' :: createUnions(xs)
            case x::Nil => List(x)
            case x::xs if (escaped) =>
                logger.trace("escaped brace operator '" + x + '\'')
                previous = x
                escaped = false
                x :: createUnions(xs)
            //range conversion
            case '-' :: xs if (!escaped)  => 
                val n = xs.head
                logger.trace("creating union range between '" + previous + "' and '" + n + '\'')
                createUnions((previous to n).toList.tail ++ xs.tail)
            case x :: xs =>
                logger.trace("adding '" + x + "' to inverse character set")
                previous = x
                x :: createUnions(xs)
        }
      def invertUnion(t: List[Char]):List[Char] = {
        var chars = (Char.MinValue to Char.MaxValue).toSet
        def invertChars(t: List[Char]):Unit = t match {
            case Nil => {}
            case '|' :: '|' :: xs => {
              chars = chars diff(Set('|'))
              invertChars(xs)
            }
            case '|' :: xs => {invertChars(xs)}
            case x :: xs => {
              chars = chars diff(Set(x))
              invertChars(xs)
            }
        }
        invertChars(t)
        chars.toList
      }
      s match {
      case Nil => Nil
      //get next brace group
      case '[':: xs =>
          val t = xs.takeWhile(c => c != ']').toList
          val rest = braceExpand(xs.dropWhile(c => c != ']').tail)
          inverse = t.head == '^'
          if(inverse) {
            logger.trace("inverting character union")
            '[' :: invertUnion(createUnions(t.tail)) ++ (']':: rest)
          }
          else {  logger.trace("expanding brace expression \"" + t + '"')
          '[':: createUnions(t) ++ (']' :: rest)  
        }
      //iterate through string
      case x :: xs =>
         x :: braceExpand(xs)
      }
    }*/

    def concatExpand(s: List[RegexToken]):List[RegexToken]= {
      val o:List[Char] = List('*','+',')')
      @tailrec
      def checkchars(s: List[RegexToken],a: List[RegexToken]):List[RegexToken] = {
        def checkfirst(c: RegexToken):Boolean = c match {
          case x if x.inRange => false
          case Input(x,false) => true
          case Operator(']',false,false) => true
          case Operator(x,e,false) if o.contains(x)||e => true
          case x => false
        }
        def checknext(c: RegexToken):Boolean = c match {
          case x if x.inRange => false
          case Operator('[',false,false) => true
          case Operator('(',e,false) => true
          case Operator(x, true, false) => true
          case Input(x, false) => true
          case x => false
        }
        s match {
          case Nil => a.reverse
          case x :: Nil => checkchars(Nil,x::a)
          case c1 :: c2 :: xs if(checkfirst(c1)&&checknext(c2)) => 
            logger.trace("adding concatenation between '" + c1.symbol + "' and '" + c2.symbol + '\'');
            checkchars(c2::xs,new Operator(backspace,false,false)::c1::a)
          case c1 :: c2 :: xs if !(checkfirst(c1)&&checknext(c2)) =>
            checkchars(c2::xs,c1::a)
        }
      }
      checkchars(s,Nil)
    }

    /*/** edits the input string to add concatenation operators ('u\0008', or backpace character)*/
    def concatExpand(s: List[Char]): List[Char] = {
      val o:List[Char] = List('*','+',')')
      var escaped:Boolean = false
      var inBrace:Boolean = s.head == '['
      /** checks for brackets or other operators and adds concatenations */
      @tailrec
      def checkchars(s: List[Char],a: List[Char]):List[Char] = {
        def checkfirst(c: Char):Boolean = c match{
          case x if inBrace => false
          case ']' if !escaped =>
            inBrace = false
            true
          case x if (isInput(c)||escaped||o.contains(c)) => true
          case x => false
        }
        def checknext(c: Char):Boolean = c match {
          case x if inBrace => false
          case '(' => true
          case '[' => 
            inBrace = true
            true
          case '\\' => true
          case x if(isInput(x)) => true
          case x => false
        }
        s match {
          case Nil => a.reverse
          case x::Nil => checkchars(Nil,x::a)
          case c1 :: c2 :: xs if(checkfirst(c1)&&checknext(c2)) =>
            escaped = c1 == '\\'
            logger.trace("adding concatenation between '" + c1 + "' and '" + c2 + '\'');
            checkchars(c2::xs, backspace :: c1 :: a)
          case x :: xs => 
            escaped = x == '\\'
            checkchars(xs,x::a)
        }
      }
      checkchars(s.toList,Nil)
    }
    */
    //###### Thompson construction algorithm ######
    /**
      * Translates the input string into a NFA
      *
      * @param s input string
      * @return (NFA of s or null,success value)
      */
    //@tailrec
    //def translateToNFA(s: List[Char]): (NFA, Boolean) = {
            /**translates a single character into a NFA using the shunting yard algorithm and adds it to the stack.*/
      /*def translateSymbol(c: Char): Boolean = {
        logger.debug("translating '" + input(c) + '\'')
        //TODO: fix bracketing
        /** handles parentheses translation */
        def parenth: Boolean = {
          logger.trace("parenth")
          if(opStack.isEmpty) throw new RegexError("mismatched brackets",r)
          else{
            //consume operators until bracket found
            while(opStack.head != '('){
              if(!eval) false
            }
            //remove bracket from stack
            opStack = opStack.tail
            true
          }
        }
        //modified shunting yard algorithm
        if (isInput(c)) {
          push(c); pushprev(c); true
        } else if(escaped){
          logger.trace("escaped operator '" + input(c) + '\'')
          escaped = false; push(c); pushprev(c); true
        } else if(c == '\\' && !escaped) {
          logger.trace("escaping next operator")
          escaped = true
          pushprev(c)
        } else if (c == '(') {
          logger.trace("adding ( to stack")
          opStack = c :: opStack; pushprev(c); true
        } else if (c == ')') parenth
        else if (opStack.isEmpty) {
          logger.trace("insert operator '"+input(c)+'\'')
          opStack = c :: opStack; pushprev(c); true
        }
        else {
          //eval operators until precedence condition or bottom of stack hit
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
      //input string consumed
      /*if (s.isEmpty) {
        //eval remaining operators
        if ((for (op <- opStack) yield eval).exists(x => x == false)) (null,false)
        if(stack.isEmpty) throw new RegexError("Translation ended with empty stack",r)
        val fsa = stack.head
        //add the final state as an accepting state
        logger.debug("accepting NFA state: " + fsa.finalState)
        fsa.finalState.accepting = true
        fsa.addAccepting(fsa.finalState)
        (fsa, true)
      } else {
        if(s.head == '['){
          val l = s.takeWhile(x => x != ']')
          pushAll(l.tail)
          translateToNFA(s.dropWhile(x => x != ']').tail)
        }
        else{
        val t = translateSymbol(s.head)
        if (!t) (null, false)
        else translateToNFA(s.tail)
        }
      }*/
      s match {
        case Nil => 
          if ((for (op <- opStack) yield eval).exists(x => x == false)) (null,false)
          if(stack.isEmpty) throw new RegexError("Translation ended with empty stack",r)
          val fsa = stack.head
          //add the final state as an accepting state
          logger.debug("accepting NFA state: " + fsa.finalState)
          fsa.finalState.accepting = true
          fsa.addAccepting(fsa.finalState)
          (fsa, true)
        case '[' :: xs if(!inBrace) => 
          inBrace = true
          translateToNFA(xs)
        case '\\' :: xs if (inBrace && !escaped) => 
          escaped = true
          translateToNFA(xs)
        case '\\' :: xs if (inBrace && escaped) =>
          escaped = false
          braceset = braceset.union(Set('\\'))
          translateToNFA(xs)
        case ']' :: xs if(escaped && inBrace) => 
          escaped = false
          braceset = braceset.union(Set(']'))
          translateToNFA(xs)
        case ']' :: xs if(!escaped && inBrace) =>
          inBrace = false
          pushAll(braceset.toList)
          translateToNFA(xs)
        case x :: xs if inBrace =>
          braceset = braceset.union(Set(x))
          translateToNFA(xs)
        case x :: xs =>
          val t = translateSymbol(x)
          if (!t) (null,false)
          else translateToNFA(xs)
      }
    }*/
    def translateToNFA(s: List[RegexToken]): (NFA,Boolean) = {
      var braceSymbols:Set[RegexToken] = Set.empty
      @tailrec
      def translateSymbols(s: List[RegexToken]): (NFA,Boolean) = {
        def translateSymbol(c: RegexToken): Boolean = {
          def parenth: Boolean = {
          logger.trace("parenth")
          if(opStack.isEmpty) throw new RegexError("mismatched brackets",r)
          else{
            //consume operators until bracket found
            while(opStack.head != '('){
              if(!eval) false
            }
            //remove bracket from stack
            opStack = opStack.tail
            true
          }
        }
          c match {
            case x if x.inRange => 
              throw new RegexError("Failed to process character set",r)
            case Input(x, false) => 
              push(x); pushprev(x); true
            case Operator(x,true,false) =>
              logger.trace("escaped operator '" + input(x) + '\'')
              push(x); pushprev(x); true
            case Operator('(', false, false) => 
              logger.trace("adding ( to stack")
              opStack = '(' :: opStack; pushprev('('); true
            case Operator(')', false, false) => parenth
            case Operator('[',false,false) => pushprev('[');true
            case Operator(']',false,false) => pushprev(']');true
            case Operator(x, false, false) if opStack isEmpty => 
              logger.trace("insert operator '"+input(x)+'\'')
              opStack = x :: opStack; pushprev(x); true
            case Operator(x,false,false) => 
              while (!opStack.isEmpty && precedence(x, opStack.head)) {
                if (!eval) false
              }
              opStack = x :: opStack
              if (stack isEmpty) false
              else {
                pushprev(x)
                true
              }
          }
        }
        s match {
          case Nil => 
            if ((for (op <- opStack) yield eval).exists(x => x == false)) (null,false)
            if(stack.isEmpty) throw new RegexError("Translation ended with empty stack",r)
            val fsa = stack.head
            //add the final state as an accepting state
            logger.debug("accepting NFA state: " + fsa.finalState)
            fsa.finalState.accepting = true
            fsa.addAccepting(fsa.finalState)
            (fsa, true)
          case x@ Operator('[',false,false)::x1::xs if x1.inRange =>
            braceSymbols = Set(x1)
            translateSymbols(x.tail)
          case x0::Operator(']',false,false)::xs if x0.inRange =>
            braceSymbols = braceSymbols.union(Set(x0))
            val b = for(c <- braceSymbols) yield (c.symbol)
            pushAll(b.toList)
            translateSymbols(xs)
          case x::xs if x.inRange =>
            braceSymbols = braceSymbols.union(Set(x))
            translateSymbols(xs)
          case x::xs if !x.inRange =>
            val t = translateSymbol(x)
            if(!t) (null,false)
            else translateSymbols(xs)
        }
      }
      translateSymbols(s)
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
      inputSet = inputSet.union(Set(c))
      //create new NFA(s0(c) -> s1)
      val s0 = new NFAState(nextId)
      val s1 = new NFAState(nextId + 1)
      nextId = nextId + 2
      s0.addTransition(c, s1)
      stack = (new NFA(List(s1, s0))) :: stack
    }

    def pushAll(l: List[Char]):Unit = {
      logger.trace("PushAll{")
      inputSet = inputSet.union(l.toSet)
      val s0 = new NFAState(nextId)
      val s1 = new NFAState(nextId + 1)
      nextId = nextId + 2
      for(c <- l) yield (s0.addTransition(c,s1))
      stack = (new NFA(List(s1,s0))) :: stack
      logger.trace("}\\PushAll")
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
        val f = new NFA(a.getStates)
        f.addStates(b.getStates)
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
      //push extra input symbol
      push(previousInput)
      //concat with star operator
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
    /** takes the epsilon closure of a set of NFAStates */
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

    /** uses the subset construction algorithm to create a DFA from the initial state of an NFA */
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
      for (state <- d.getStates if(state.deadEnd)) yield {
        logger.trace("Removing " + state);
        d.removeState(state)
        for (s <- d.getStates) yield {s.removeTransitions(state)}}
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
      val s = makeSymbols(r.toList,Nil)
      logger.debug("symbol list: " + s)
      val b = braceExpand(s)
      logger.debug("converted braces: "+b)
      val c = concatExpand(b)
      logger.debug("converted concatenations: "+c)
      escaped = false
    //Thompson construction
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
    //subset construction
    val d = dTranslate(nfa initialState)
    logger.debug("included states: "+d.getStates)
    logger.whenTraceEnabled{for(s <- d.getStates) yield {logger.trace(s"State: " + s + ", accepting: " + s.accepting)}}
    dfaReduce(d)
    }
  }
}

private abstract class RegexToken(val symbol: Char, val inRange: Boolean) {
  override def equals(x: Any): Boolean = {
    if (!x.isInstanceOf[RegexToken]) false
    else{
      val s = x.asInstanceOf[RegexToken]
      this.symbol == s.symbol && this.inRange == s.inRange
    }
  }
  override def toString(): String = symbol.toString + ":" + inRange
}

private case class Operator(s: Char,escaped: Boolean,r: Boolean) extends RegexToken(s,r) {
  override def equals(x: Any): Boolean = {if (!x.isInstanceOf[Operator]) false
    else {
      val s = x.asInstanceOf[Operator]
      this.symbol == s.symbol && this.escaped == s.escaped && this.inRange == s.inRange
    }
  }
  override def toString(): String = if(escaped) "\\"+symbol + ":" + inRange else symbol + ":" + inRange
}

private case class Input(s: Char, r: Boolean) extends RegexToken(s,r)

/**
  * An error class indicating that the translation of a Regular Expression has failed
  *
  * @param msg reason for failure
  * @param input regex input symbols
  */
class RegexError(msg: String,input: String) extends Error("Failed to translate Regular Expression: " + msg + ", input:\"" + input + '"') with LazyLogging{
  logger.error("Failed to translate Regular Expression: " + msg + ", input:\"" + input + '"')
}