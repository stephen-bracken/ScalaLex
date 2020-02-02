# Lexical analyzer generator
This tool is designed to implement a subset of the [LEX](http://dinosaur.compilertools.net/lex/) specification.

# Regular Expressions
Currently, supported regular expression operators are:
    [ ] ( ) \ * + |
    [] - creates a set of union operations between characters. Includes support for character ranges, e.g.
        [0-9] is equivalent to (0|1|2|3|4|5|6|7|8|9)
    () - standard bracketing
    \  - escape character
    *  - kleene star, i.e. 0 or more of previous symbol
    +  - like kleene star, but with 1 or more, e.g. a+ is equivalent to aa*
    |  - union/or operation, i.e. previous symbol or next symbol
