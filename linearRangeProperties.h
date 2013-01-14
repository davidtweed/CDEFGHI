#ifndef __LINEAR_RANGE_PROPERTIES__
#define __LINEAR_RANGE_PROPERTIES__

#include "smallBitvectors.h"
#include <algorithm>

#define AND_LAMBDA [] (uint8_t x,uint8_t y) { return x & y;}
#define OR_LAMBDA [] (uint8_t x,uint8_t y) { return x | y;}
#define MIN_LAMBDA [] (uint8_t x,uint8_t y) { return std::min(x,y);}
#define MAX_LAMBDA [] (uint8_t x,uint8_t y) { return std::max(x,y);}

#define IDX2(d,l2l,i) (((d)<<(l2l)) | (i))

extern int log2T[256];

void formFwdTable(uint8_t *elts,int len,int l2l,int depth);

void formBwdTable(uint8_t *elts,int len,int l2l,int depth);

void formUnionTable(uint8_t *elts,int len,int l2l,int depth);

void formIntersectTable(uint8_t *elts,int len,int l2l,int depth);

uint8_t andValueOverRange(uint8_t *tbl,int l2l,int start,int end);

uint8_t orValueOverRange(uint8_t *tbl,int l2l,int start,int end);

uint8_t maxValueOverRange(uint8_t *tbl,int l2l,int start,int end);

uint8_t minValueOverRange(uint8_t *tbl,int l2l,int start,int end);


//inline Byte ByteAnd(Byte a, Byte b) { return a & b; }

#endif /*__LINEAR_RANGE_PROPERTIES__*/
