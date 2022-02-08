## Language Reference
### Keywords
* fn
* extern

### Data Types
* int (i32)
* String (i8 ptr)
* void

### Operations
* \+ Plus
* \- Minus
* \* Mul
* / div

## Grammar
* \<top-level-expression> ::= \<extern> | \<function>
* \<extern> ::= extern fn\<ident>(\<ident>:\<ident>, ...) -> \<ident>;
* \<fn> ::= fn\<ident>(\<ident>:\<ident>, ...) -> \<ident> { (\<expr>;)* }
* \<ident> ::= [a-zA-Z]+