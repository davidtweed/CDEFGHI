#ifndef __LINEAR_RANGE_PROPERTIES__
#define __LINEAR_RANGE_PROPERTIES__

#include "smallBitvectors.h"



template<typename T,typename F,int FIN_SZ>
inline T rangeLookup(T** tbl,int start,int end) {
    int depth=log2[end-start];
    return F(T[depth][FIN_SZ*start],T[depth][FIN_SZ*(end-(1<<depth))]);
}

inline Byte ByteAnd(Byte a, Byte b) { return a & b; }

#endif /*__LINEAR_RANGE_PROPERTIES__*/
