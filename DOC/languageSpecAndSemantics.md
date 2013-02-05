# Overriding principle #

The general principle is to avoid specifying anything which might prevent generation of efficient code. Secondary to that, if there's anything it would be helpful to the user which could be made specified, we attempt to do that.

## Rationales ##

_PER_: _Principle of Efficient Running_. The given rule leads to most efficient running code being produced.

_PEC_: _Principle of Efficient Compilation_. The given rule leads to mostleads to most efficient just in time compilation.

_PMJW_: _Principle of Most likely to Just Work_. The given rule is (considered to be) the one most likely to "just work" for the majority of user-level code. This is an admission that there it is the best choice but that there remains a degree of both arbitrariness and that no choice can be both efficient and completely correct.

_PLS_: _Principle of least surprise_. The given rule is (considered to be) the most consistent with other features of the language.

## Types of behaviour ##

_Unspecified_ behaviour means that anything can happen in the immediately unspecified element, but other pars of the program are unaffected. (This is "undefined but not contaminating anything else.)

_Undefined_ behaviour refers to behaviour which renders the program as a whole completely undefined (analogous to the way C's undefined behaviour is allowed to do anything).

## Type conversions ##

The language deals with two kinds of element: scalar types (which roughly denote one "thing") and arrays of a given _rank_, _shape_ which contain elements of some _specific_ scalar type. (Arrays containing variable types of element are not permitted.)

The following refers to numeric types.

### Scalar type "width" ###

The width of a scalar type is the number of bits in its "lowest level" representation, eg, a 16 bit signed and unsigned  integers have a width of 16.

### Type propagation ###

The type of a left-hand-side variable is propagated from the types of its right-hand side constituents and the operations used, unless it is given a specific type. However, type inference (propagating backwards in the program) is _not_ done. The types of the input variables are known by virtue of the types of the concrete objects they're called with.

### Implicit type conversions ###

**Conversions** refer to simple conversions of one type to another which will work correctly if the value is representable in both the before and after types, but behaviour is unspecified if the result type cannot represent the original value. Rounding modes going from floating point to integer are unspecified.

If a binary operation of polymorphic type $T\times T\righarrow T$ is applied to scalars $w$ and $v$ of widths $W$ and $V$ respectively, then T is chosen to be the wider of width $W$ and $V$, and is unsigned if both $w$ and $v$ are unsigned and signed otherwise. (PMJW)

If a binary operation of polymorphic type $T\times T\righarrow T$ is given an explicit type annotation $::W$ then the arguments are converted to type $W$ (regardless of their current types) and the operation of type  $W\times W\righarrow W$ is applied. This is a mechanism for sepcifying the type _during evaluation_. (Rationale: there may be data which is adequately represented in a given type but it may be known that a specific larger type is needed to avoid overflows in evaluation. LIkewise the "data in memory" may be in a bigger type for some (possibly legacy) reason, but it may be know the actual values will fit into smaller types during evaluation.)

If a right-hand side has an explicit type annotation then it is treated as if it had that type for the rules above, and the read of the variable will be followed by a conversion to that type.

If a left hand side has an explicit type annotation then the value from the right hand side is computed normally, then converted to the specified type before storage. In addition, the type is the type of that variable when used in future right-hand sides.

#Operation ordering/precedence#

The operators $+$, $-$, $*$, $/$, $^$ all retain their mathematical precedence. (_PLS_, since APL/Q's unspecified ordering is different to printed formulae it raises risk of inadvertant bugs when implementing.)  Any other operators are interpreted as right-most expression innermost. (_PIC_).

#"Implicit loop" ordering and fusion#