# Lexical analyzer generator
This tool is designed to implement a subset of the [LEX](http://dinosaur.compilertools.net/lex/) specification.

# Regular Expressions
**Supported operators**

   Currently, supported regular expression operators are:
        
        [ ] ( ) \ * + |
        
- ``[]`` creates a set of union operations between characters. Includes support for character ranges, e.g.
            
    - ``[0-9]`` is equivalent to (0|1|2|3|4|5|6|7|8|9)
        
- ``()`` standard bracketing
        
- ``\``  escape character
        
- ``*``  kleene star, i.e. 0 or more of previous symbol
        
- ``+``  like kleene star, but with 1 or more, e.g. a+ is equivalent to aa*
        
- ``|``  union/or operation, i.e. previous symbol or next symbol

**Planned operators**
- ``{}`` used for definitions or specifying a specific number of symbols, e.g. 
    - ``a{1,3}`` matches 1 or 3 instances of a
    - ``{digit}`` looks for a rule with the name digit
- ``/`` requires matching of following symbols, e.g.
    - ab/cd matches ab only if followed by cd
- ``<def>`` indicates start conditions
