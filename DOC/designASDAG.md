_NOTE_: The current ASDAG code doesn't use any Julia specific data. One reason for this is that we want to avoid doing any checks/processing which we'd normally need to do, but aren't necessary given that we know this code is valid (typically because it has been run successfully before). Hopefully a very thin wrapper function can be generated later.

The basic design goals for the ASDAG (abstract syntax directed acyclic graph) are:

1. Expresses array operations and types in terms of how new elements, either conecptually or actually, of a result array at a given "combinations of indices" is generated from inputs at "combinations of indices".

2. Makes it easy to see when a computed value is being reused (so it's not recomputed).

3. Makes it traversal and/or reduction over a given dimension explicit.

4. Fits well with LLVM codegen. In particular, any optimization that LLVM can reliably do is left for it to do, we try and only exploit higher-level knowledge that's not readily available once code is lowered to LLVM.

The ASDAG-compilation code is responsible for generating good code that follows the DAG, but does not do any transformations which would actually alter the DAG. (It's assumed such transformations will be done elsewhere if desired.)

The basic idea is to construct a DAG from subclasses of ExprCpt nodes which
contain pointers to other ExprCpt nodes, where the "leaves" are all either literal values or elements of an input array, and the "root" of the DAG is the outermost traversal. Since multiple ExprCpts can point to a given ExprCpt, there is an llvmTemp field which is initially null and is set to the SSA Value* once code has been generated for that node. If llvmTemp has been set _it is invalid to
process the node again_; the llvmTemp value must instead be used. (This is important because some operations create a phony ExprCpt with just a set llvmTemp value.)

For simplicity, _all_ intermediate values are "viewed as" part of an array. However, each node has a switch determining if the value should actually be written to the array when computed. If not then the value is only _conceptually_ an array element but the temporary array is never actually written to or read from, so needn't exist. The idea is partly to make it simpler to debug things by being able to "instantiate" arrays in an ASDAG without needing to rebuild it. As such, there's a "read" node class (VarRec) but no separate "write" node class.


* _VarRec_: a read from one of the input arrays.
* _UFnApp_: application of a unary function to an array (eg, abs, etc). _UNIMPLEMENTED_
* _BinaryCombiner_: takes two ExprCpts and applies a binary operation to them to produce a result.

* _Traversal_: takes a list of ExprCpt representing the body and a list of ExprCpt denoting "accumulations" (aka folds, reductions, etc). In code generation, first all the ExprCpt's reachable from the body list are processed, then the list of accumulations. The accumulation ExprCpts _MUST_ be a simple binary operator with one argument being an ExprCpt _which has already been evaluated somewhere within the body list_. This is because processing accumulations requires setting phi-nodes (which need to know about BasicBlock structure) and processing certain ExprCpt nodes triggers the creation of new LLVM BasicBlocks during the process. Having this happen would generated screwed up (probably invalid) LLVM code.

# Implementing where clauses #

