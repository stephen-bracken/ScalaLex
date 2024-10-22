# ScalaLex
*The following readme file is supplied with ScalaLex and is available on [Github](https://github.com/stephen-bracken/ScalaLex)*

This tool is designed to implement a modified version of the [LEX](http://dinosaur.compilertools.net/lex/)/[FLEX](http://dinosaur.compilertools.net/flex/manpage.html) specification. This program implements all of the standard functions of lex or flex, but the output program is written in scala.

ScalaLex uses a Regex Compiler that produces DFA equivalents of each of the regex rules in the output program. Each Regex is matched using longest prefix match, and in the case of two or more matches of the same length, the first matched rule is used.

## Requirements
This tool was built using sbt and requires Java 8 to run.

## Instructions for use
To use this program, you need to first package it into a jar file by running the command ``sbt package`` (or just ``package`` if inside sbt), which should create a jar file in the /scalaLex/target/scala-2.12 directory. 

This file needs to be added to your system classpath to allow the output program to run. To do this you need to add the jar file to your build file using IntelliJ or sbt (for sbt see https://www.scala-sbt.org/1.x/docs/Classpaths.html), to run the tool using the sbt scala interpreter run the command: ``set fullClasspath in Compile += Attributed.blank(file(".../ScalaLex/target/scala-2.12/ScalaLex.jar"))`` **note that this alone will not enable the output file to run as you must do the same thing in a separate sbt build file**

You can check your classpath by using the command ``show compile:fullClasspath``

The arguments for ScalaLex are as follows:

    - -i : the input file to read defninitions from
    - -o : the name of the output source file to be created

e.g.
running ``java -jar scalaLex -i input.txt -o output`` will read definitions from the source file input.txt (an example has been provided for you) 
and output to the file /output/output.scala. Note that ScalaLex will always produce a file named dfa which is **always necessary** for running the output program.

You can extract the contents of the output folder anywhere you want and create a new sbt build there.

### sbt build file
To resolve dependencies in the output program, you will need to use a build tool such as sbt or IntelliJ, and add the following dependences to the output program in a build file (build.sbt for sbt):

```
libraryDependencies ++= Seq(
    "org.slf4j"%"slf4j-api"%"1.7.25", //simple logging facade for Java
    "ch.qos.logback"%"logback-classic"%"1.2.3", //logback
    "ch.qos.logback"%"logback-core"%"1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2", //scala-logging
)
```

## Input Language

The top level syntax for an input file is:


    {defs}
    %%
    {rules}
    %%
    {user subroutines}


the subroutines section can be ommitted, therefore the minimum input specification is

```
    
    %%
```

which will produce a program that will simply output any input given.


### Definitions, Options and States
the first section {defs} may contain any of the following expression types:


- ``%option `` Lexing options for the generated lexer

- ``%x INSTRING INCOMMENT`` lexing states to enable/disable rules that are either exclusive (%x) or inclusive (%s)

- ``{name} regex`` associates a name with a regex. The name can be called in the rules section e.g. ``{number} [0-9]+`` can be called using ``{number}`` and the call will be replaced with ``[0-9]+`` in the output regex

- ``    println("example code");`` indented lines will automatically be treated as code that will be executed when the lexer is initialised. Code in this section is placed before definition of the lexer class so any code here aside from imports should be wrapped in a class or object to prevent compiler errors.

- ``/* comment */`` /* and */ can be used to declare a comment.

### Rules
a rule is of the form 
```
<StartCondition> Regex    action
```

start conditions may be omitted but there must always be a regex that ends with a tab space. Actions are comprised of arbitrary scala code. If the start condition is omitted, it is assumed to be ``<INITIAL>``, which is the default state of the lexer if no states are added in the defs section. Switching lexing states can be done in actions.

#### example
```
hello    println("hello")
```

this rule uses the regex ``hello`` which matches the word "hello", and as it's action, it prints "hello" to the console.

### Subroutines
the user subroutines section consists of a continuous code block that can be filled with any additional functions that you want to reference using either rules or the initial code. These will be included in the lexer class at the bottom.

## Built-in methods
the following methods are built into the output file to allow for utility functions and define a standard pipeline for lexing:

- ``readFile()`` reads the file from the supplied location and writes it into the input string.

- ``input()`` gets the next character in the input stream

- ``output()`` writes a character to the output stream

- ``unput()`` writes a character back onto the front of the input stream

- ``yylex()`` runs each regex on the input string and consumes the longest matched string. If an action is associated with the regex, then the action method is called.

- ``doRule(r)`` is used by yylex to call code actions.

# Regular expression support

## Regex Operators

- ``{x,y}`` matches the previous symbol a minimum of x times and a maximum of y times. the max parameter is optional

    - ``a{1,3}`` matches 1 to 3 instances of a

- ``[]`` creates a set of union operations between characters. Includes support for character ranges, e.g.
            
    - ``[0-9]`` is equivalent to (0|1|2|3|4|5|6|7|8|9)

    
    - ``^`` inverts the character set, matching any character **except** the characters in the set, e.g.
    - ``[^0-9]`` matches any character except ``[0-9]``
        
- ``()`` standard bracketing
        
- ``\``  escape character
        
- ``*``  kleene star, i.e. 0 or more of previous symbol
        
- ``+``  like kleene star, but with 1 or more, e.g. a+ is equivalent to aa*
        
- ``|``  union/or operation, i.e. previous symbol or next symbol

- ``"``  quote sequences are used to escape all operators in a sequence, e.g. "xyz++" is equivalent to the input sequence xyz++

- ``/`` lookahead operator - when translated this becomes a concatenation, because the automata always matches all of the input sequence*

*note that it is possible to get partial matches from the getMatches and longestPrefixMatch functions, but the lookahead functionality remains the same.*

### Special operators
the following operators are used during the processing of an input string, and will be escaped.

- ``\u0008`` is a backspace character that is used to represent concatenations

- whitespace is used for parsing rules and definitions, so to use whitespace in your regex, it is highly reccomended to parenthesise it using ``[ ]``

### Planned features

- ``/`` trailing context - ``a/b`` matches ab but returns b to the input

- ``$`` special case lookahead - checks that the entire input has been consumed. Equivalent to ``x/\n``

- ``^`` lookbehind - checks that the beginning of the string was matched

- ``.`` matches any character

- ``{x,}`` matching x or more characters

- multiple start state matching, e.g. ``<INSTRING,INCOMMENT>x``

- ``<<EOF>>`` end of file

- character classes, e.g. ``[:alnum:]`` = ``[]``

- ``REJECT;`` rejects this regex and moves onto the next best match.
