#ifndef __SMALL_BITVECTORS_H__
#define __SMALL_BITVECTORS_H__

#include <stdint.h>

typedef uint32_t SBitvector;

inline SBitvector cup(SBitvector a,SBitvector b) { return a | b; }
inline SBitvector cap(SBitvector a,SBitvector b) { return a & b; }


inline int log2(SBitvector v) { return 0; }

#endif /*__SMALL_BITVECTORS_H__*/
