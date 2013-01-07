#ifndef __SMALL_BITVECTORS_H__
#define __SMALL_BITVECTORS_H__

#include <stdint.h>

typedef uint32_t SBitvector;

inline SSBitvector cup(SBitvector a,SBitvector b) { return a | b; }
inline SSBitvector cap(SBitvector a,SBitvector b) { return a & b; }


#endif /*__SMALL_BITVECTORS_H__*/
