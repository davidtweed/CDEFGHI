#ifndef __TINY_GRAPH__
#define __TINY_GRAPH__

/*SUBTLE POINT: for a program with at most k-ary operations, each of n nodes has at most k parents and there are at most
 *kn edges in the graph (in contrast to the worst case general graph with O(n) parents and O(n^2) edges)
 */
#include <emmintrin.h>

typedef union {
    uint32_t wrd[4];
    __m128i simd;
} Int4;

struct TinyGraph {
    U8 parents[32][3];
    int noNodes,noEdges;
    uint32_t weights;
    Int4 edgeReps[0];
};

int positionInWord;

inline
int redundantEdge(Int4 hdMask,Int4 tlMask,Int4 heads,Int4 tails) {
    __m128i zeroes=_mm_setzero_si128();
    _mm_or_si128(_mm_cmpeq_epi32(_mm_and_si128(hdMask,heads),zeroes),
                 _mm_cmpeq_epi32(_mm_and_si128(tlMask,tails),zeroes));

#endif /*__TINY_GRAPH__*/
