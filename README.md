# Typechecker for System F (polymorphic lambda calculus)
![Test CI](https://github.com/muldrik/polymorphic-lambda-typechecker/actions/workflows/haskell.yml/badge.svg)

This project implements convenient input of [System F](https://en.wikipedia.org/wiki/System_F) types and expressions as well as CLI for typechecking.

It can also be used for [Simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) if no specific syntax for System F is used

## Examples of successful typechecks
```

    $ cabal run 'typechecker-executable' "x:a" "x" "a"
    $ cabal run 'typechecker-executable' "x : (a->b->c), y : a, z : b" "x y z" "d"   
    $ cabal run 'typechecker-executable' "" "(#a. \x:a . x) {beta}" "beta -> beta"
    $ cabal run 'typechecker-executable' "" "(#a. \x:a . x)" "@t. t -> t"
```
    
All of these should result in "Typecheck successful" message appearing and program exiting correctly

## Examples of errors and unsuccessful typechecks:
```
    $ cabal run 'typechecker-executable' "" "x" "a"
    "Type inference error: Free variable x not found in context"
```   
Type inference error appears when the expression cannot be typed correctly in an way and provides an explanation on where the process terminated

```
    $ cabal run 'typechecker-executable' "" "1var" "a"  
    "Parsing error: Failed to parse the expression argument"
```
Parsing error occurs when environment, expression or expected typed cannot be parsed. In this case - variable name cannot start with a number

```
    $ cabal run 'typechecker-executable' "x : a" "x" "b"   
    "Typechecking failed: actual expression type is a which is not alpha-equivalent to expected type b"
```
Typechecking failed message appears when the inferred expression type is not alpha-equivalent to the given expected type

Examples of alpha-equivalent types:
* `@a. a` and `@b. b`
* `@a. @b. a -> b` and `@t1 t2. t1 -> t2`

## Overview


The program can be ran in a single mode expecting 3 arguments:

    cabal run 'typechecker-executable' "<environment>" "<expression>" "<expected type>"

Environment must specify the types of all free variables used in a given expression. 

Type must be non-empty and match the following syntax:

1) A type variable is a type. Valid variable name starts with a letter, followed by letters, digits, underscores(_) and apostrophes('). Examples: 
*`myVar`
*`var'`
*`Also_Valid`

2) <type> -> <type> is a type. Examples: 
* `alpha -> beta`
* `(a -> b) -> c`
* `(a -> a) -> (b -> b)`

3) @<type variable>. <type> is a type. @ is a replacement for the 'forall' math quantifier. Examples: 
* `@a.a`
* `@a. @b. (a -> b)`

Expression must be non-empty and match the following syntax:

1) A variable is an expression. Allowed names are the same as type variables

2) A lambda abstaction is an expression. General syntax: `\<variable name>` : `<variable type>`. `<expression>`. Examples: 
* `\x: alpha. x`
* `\x: a. \y: (b -> a). x y`

3) An application in an expression. General syntax: `<exp1> <exp2>`. Examples: 
* `x y`
* `x y z`
* `(\x : a. x) y`

4) A type abstraction is an expression. General syntax: `#<type variable>. <expression>`. # symbol is a replacement for the more common big lambda notation. Examples: 
* `#a. \x:a . x`
* `#a. #b. lol`

5) A type application is an expression. General syntax: `<expression> {<type>}`. Examples: 
* `(#a. \x:a.x) {beta}`
* `z {gamma}`



### Notes
1) The parser is very liberal to extra brackets and spaces. Brackets are not allowed only around lambda, type lambda or forall arguments. For example, `( (\x   :((a) -> ((B))) .  x))` is a valid expression and `((a) ) -> ( b)` is a valid type

2) Chaining lambdas, type lambdas and @ quantifiers can be combined for easier use. Examples:
* `\x:a y:b z:c. z` is equivalent to `\x:a . \y:b. \z:c . z`
* `#a b c. a b c` is equivalent to `#a. #b. #c. a b c`
* `@a b c. a -> b -> c` is equivalent to `@a. @b. @c. a -> b -> c` 


### Testing

To execute the tests locally, run

    cabal new-test --test-show-details=streaming
    
### Type variable shadowing

Lambda variable shadowing poses no problems as no actual applications have to be actually performed. Therefore, expressions such as `\x:a \x:b . x` are valid (this one has type `a -> b -> a`)

An effort was made to also allow all kinds of type variable shadowing. However, one has to be cautious when shadowing type variables, especially free type variables from the environemnt as this only makes the result difficult to read and leaves room for easy mistakes.

For example:
```
    $ cabal run 'typechecker-executable' "" "#a. (\x: a. \ y: (@a. a). x)" "@t1. t1 -> (@t2. t2) -> t1"
    "Typecheck successful"
```

