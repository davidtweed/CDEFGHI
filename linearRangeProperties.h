#ifndef __LINEAR_RANGE_PROPERTIES__
#define __LINEAR_RANGE_PROPERTIES__

#include "smallBitvectors.h"

void formFwdTable(uint8_t ** elts,int len,int depth);

void formBwdTable(uint8_t ** elts,int len,int depth);

void formUnionTable(uint8_t ** elts,int len,int depth);

void formIntersectTable(uint8_t ** elts,int len,int depth);

template<typename T,typename F,int FIN_SZ>
inline T valueOverRange(T** tbl,int start,int end) {
    int depth=log2[end-start];
    return F(T[depth][FIN_SZ*start],T[depth][FIN_SZ*(end-(1<<depth))]);
}

inline Byte ByteAnd(Byte a, Byte b) { return a & b; }

#endif /*__LINEAR_RANGE_PROPERTIES__*/
