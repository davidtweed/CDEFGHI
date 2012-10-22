The language is based around a basic semantics of, as much as possible, each statement being interpretable as an individual loop but with the aim of fusing/removing as many of those loops as possible. (Some elements, in particular conditional reductions, unavoidably require the basic semantics to refer to multiple elements.)

Here's a very simple routine:

~~~~
dC.ij! = d.ij - m.j
pVec.ik! =+ dC.i% * v.k%
predVec.ij! =+ pVec.i% * v.j%
residDist.i! =+ (dC.i% - residVec.i%)^2
within.i! = residDist.i <= lSq
where within.i=>I{
  newSum.j =+ dC.Ij
  newCov.jk =+ dC.Ij*dC.Ik
  count =+ 1
}
~~~~

Here
    dC.ij = d.ij - m.j
has the meaning:

* identify the rangs $i_R$ for $I$ and $j_R$ for $j$.
* for all indices $i\in i_R$, indices $j\in j_R$, set $dC[i,j]$ to $d[i,j]-m[j]$.

The _**only**_ role the alphabetical indices play is to indicate the matching of "slots" between different arrays. (This could be done using other syntaxes for specification, but this is probably the simplest.) Note that because the "array variables" arrive as input along with dimension specifications, it's possible to automatically propagate the ranges from the right-hand side to its left hand side, and hence throughout the program. However the "loops" implied by the "for all" are explicitly unordered, and potentially even parallel. The mini-language does not support assignments in which array values depend on other values being computed in a particular order.

The line
    pVec.ik! =+ d.i% * v.k%
denotes a _reduction_, indicated by the assignment "$=$" being followed immediately by an operator, in this case "$+$". The index variable % is a special variable that indicates "reduce over this index", the other indices have their standard roles of indicating matching slots. In detail, this line has the meaning:

* identify the rangs $i_R$ for $I$, $k_R$ for $k$ and $%_R$ for $%$.
* for all indices $i\in i_R$, indices $k\in k_R$, set $pVec[i,k]$ to the sum over new variable $%$ in $dC[i,%]*v[k,%]$.

Note that it is deliberately unspecified the order in which all of these implied loops occur, whether values are accumulated in registers or accumulated via memory, etc. Again, the minilanguage cannot handle any algorithms which require a specific summation order for numerical stability, etc.

Also note that indices only apply within a line and there is no need to be consistent between lines for the compiler, eg, in

    pVec.ik! =+ dC.i% * v.k%
    predVec.ij! =+ pVec.i% * v.j%

the $v.k%$ and $v.j%$ are both referring to the same pattern. This is possible because we know the set of array indexes and bounds on the right-hand side, so that there's no lack of precision from using different indices. Although it's generally helpful to be consistent, particularly for other readers of the code, this approach is chosen to make modfications and extensions to existing code as easy as possible without needing to change all index variables due to a clash.

One thing that hasn't been mentioned so far is the appearance of '!' markers on variables on the left-hand side of assignments. This denotes a "conceptual array", in the sense that there is no requirement for an _actual, in-memory_ array to be allocated and filled with the values providing that all the "normal arrays" that depend, either directly or indirectly, on the conecptual array end up filled with the correct values. This mechanism is provided for two reasons:

1. It's often natural to think of an algorithm in terms of intermediate arrays, even though they don't actually need to exist in terms of a "input/output" view of the routine.
2. Using a simple marker makes it easy to temporarily remove it so that the array is available for debugging purposes.

The compiler will complain if it is unable to construct a suitable program without needing to actually create an in-memory verison of a conceptual array.

Finally, a note about occurrences of index variables on the left and right-hand side of assignents. There are only two cases when an index can occur on the right-hand-side of an assignment but not the left:

1. If it is a literal integer.
2. In a reduction, where the variable is being "reduced over".

Likewise, an index variable can only appear on the left-hand side and not the right-hand side if it is a literal integer.

These rules are requirements for array size determination to be simple tracking, rather than requiring a full inference.