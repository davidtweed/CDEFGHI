# Introduction #

The mini-language is based around a basic semantics of, as much as possible, each statement being interpretable as an individual set of loops but with the aim of allowing transformations fusing/removing as many of those loops as possible. (Some elements, in particular conditional reductions, unavoidably require the basic semantics to refer to multiple elements.) Indeed a major goal of the language is to make determining which elements are actually independent (as opposed to dependent, which is the "conservative" assumption) as easy for the JIT-compiler as possible.

# Philosophy #

Before diving into an example, couple of philosophical principles will help understand some of the language design decisions.

* "Vectorised code" (in the sense of Matlab or APL) _**optimised by the compiler**_ is preferable to manually written loops because
    1. It's often easier to modify/extend vectorised code than carefully tuned loops.
    2. On modern systems the optimal "loop structure" often depends on the precise CPU and architecture; writing vectorised code potentially allows the compiler to make choices on the basis of the particular machine it's running on.

* Systems based on higher order functions like map, fold, scan (such as APL or Haskell) look very elegant for 1-D analysis, less elegant in 2-D and very intricate in 3-D. This is particularly true when the data-source is so large it has to be consumed in the producers format and can't be, eg, transposed, to match another structure without large runtime costs. Likewise the implicit "whole object rules" (like Matlab's "matrix + scalar" becomes matrix of "each matrix element+scalar") work well for 2-D linear algebra style stuff, but less well in more general settings.

# A running example #

Here's a very simple routine:

~~~~
dC.ij! = d.ij - m.j
pVec.ik! =+ dC.i% * v.k%
predVec.ij! =+ pVec.i% * v.j%
residDist.i! =+ (dC.i% - residVec.i%)^2
where I => residDist.i <= lSq {
  newSum.j =+ dC.ij
  newCov.jk =+ dC.ij*dC.ik
  count =+ 1
}
~~~~

which will be used as a running example to explain elements of the language.

## Simple assignments ##

Here
    dC.ij = d.ij - m.j
has the meaning:

* identify the range $i_R$ for $i$ and $j_R$ for $j$.
* for all indices $i\in i_R$, indices $j\in j_R$, set $dC[i,j]$ to $d[i,j]-m[j]$.

The _**only**_ role the alphabetical indices play is to indicate the matching of "slots" between different arrays. (This could be done using other syntaxes for specification, but this is probably the simplest.) Note that because the "array variables" arrive as input along with dimension specifications, it's possible to automatically propagate the ranges from a statement's right-hand side to its left hand side, and hence throughout the program from the initial inputs. However the "loops" implied by the "for all" are explicitly unordered (as is the relative order between iteration in $i$ and $j$), and potentially even parallel. The mini-language does not support assignments in which array values depend on other values being computed in a particular order (but does have the obvious straightforward ependence semantics: $a.ij=b.i$ has, for each $I$ and $J$, that $a[I,J]$ depends upon $b[I]).

## Reductions ##

The line
    pVec.ik! =+ d.i% * v.k%
denotes a _reduction_, indicated by the assignment "$=$" being followed immediately by an operator, in this case "$+$". The index variable % is a special variable that indicates "reduce over this index", the other indices have their standard roles of indicating matching slots. In detail, this line has the meaning:

* identify the range $i_R$ for $i$, $k_R$ for $k$ and $%_R$ for $%$.
* for all indices $i\in i_R$, indices $k\in k_R$, set $pVec[i,k]$ to the sum over $%$ in  $%_R$ of $dC[i,%]*v[k,%]$.

Note that it is again deliberately unspecified the order in which all of these implied loops occur, whether values are accumulated in registers or accumulated via memory, etc. Again, the mini-language cannot handle any algorithms which require a specific summation order for numerical stability, etc.

Also note that indices only apply within a line and there is no need to be consistent between lines for the compiler, eg, in

    pVec.ik! =+ dC.i% * v.k%
    predVec.ij! =+ pVec.i% * v.j%

the $v.k%$ and $v.j%$ are both referring to the same pattern. This is possible because we know the set of array indexes and bounds on the right-hand side, so that there's no lack of precision from using different indices. Although it's generally helpful to be consistent, particularly for other readers of the code, this approach is chosen to make modfications and extensions to existing code as easy as possible without needing to change all index variables due to a clash.

## Conceptual arrays ##

One thing that hasn't been mentioned so far is the appearance of '!' markers on variables on the left-hand side of assignments. This denotes a "conceptual array", in the sense that there is no requirement for an _actual, in-memory_ array to be allocated and filled with the values providing that all the "normal arrays" that depend, either directly or indirectly, on the conecptual array end up filled with the correct values. This mechanism is provided for two reasons:

1. It's often natural to think of an algorithm in terms of intermediate arrays, even though they don't actually need to exist in terms of a "input/output" view of the routine.
2. Using a simple marker makes it easy to temporarily remove it so that the array is available for debugging purposes.

The compiler will complain if it is unable to construct a suitable program without needing to actually create an in-memory version of a conceptual array.

## Where blocks ##

A where block provides a simple means of applying operations, particularly reduction operations, only at a set of indices wich correspond to some condition being met. In particular, the simplified example

~~~~
where I => residDist.i <= lSq {
  newSum.j =+ dC.ij
  count =+ 1
}
~~~~

is equivalent to

* identify the range $i_R$ for $i$
* construct a list $L$ with one entry $i$ for $i$ in $i_R$ where $residDist[i] <= lSq$
* reductions have their previous semantics, except that instead of being over all $i_R$ it's just over the entries in L. So in paritcular:
    + find the range $j_R$ for $j$ and, for each $j$ set $newSum[j]$ to the sum over $i$ in L of $dC[i,j]$.
    + set count to the sum over $i$ in L of $1$ (which equals $length(L)$.

A further wrinkle is that the list $L$ implicitly defines _two_ variables: $i$ which is the value in $L$ (i.e., the original index where the condition held) and $I$ (the upper case version of the "original index" variable) which corresponds to the current position in $L$. E.g., if L=[1,4,7,9] then when $i=7$, $I=3$. In the above case $I$ is not used in the body. (This will become more important in a couple ofadvanced examples of where blocks discussed later.)

## Rules about where indexes can appear ##

Finally, a note about occurrences of index variables on the left and right-hand side of assignents. There are only two cases when an index can occur on the right-hand-side of an assignment but not the left:

1. If it is a literal integer.
2. In a reduction, where the variable is being "reduced over".

Likewise, an index variable can only appear on the left-hand side and not the right-hand side if it is a literal integer.

These rules are requirements for array size determination to be just simple forward propagation, rather than requiring a full graph-based inference.
