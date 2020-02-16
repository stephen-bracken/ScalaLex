# Lexical analyzer generator
This tool is designed to implement a subset of the [LEX](http://dinosaur.compilertools.net/lex/) specification.

ScalaLex uses a Regex Compiler that produces DFA equivalents of each of the regex rules in the output program. Each Regex is matched using longest prefix match, and in the case of two or more matches of the same length, the first matched rule is used.

# Input Language
**operators**

   Currently, supported operators are:
        
        [ ] ( ) \ * + | " %

- ``%%`` is used to delimit sections. The top level syntax for an input file is
    ```
    {defs}
    %%
    {rules}
    %%
    {user subroutines}
    ```
    subroutines can be ommitted, therefore the minimum input specification is
        ``%%``
    which will produce a program that will simply output any input given.

**Regex Operators**
        
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

**Special operators**
the following operators are used during the processing of an input string, and will be escaped.

- "\u0008" is a backspace character that is used to represent concatenations

**Planned operators**
- ``{}`` used for definitions or specifying a specific number of symbols, e.g. 
    - ``a{1,3}`` matches 1 or 3 instances of a
    - ``{digit}`` looks for a rule with the name digit
- ``/`` requires matching of following symbols, e.g.
    - ab/cd matches ab only if followed by cd
- ``<def>`` indicates start conditions
