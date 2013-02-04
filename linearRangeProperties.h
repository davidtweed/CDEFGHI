#ifndef __LINEAR_RANGE_PROPERTIES__
#define __LINEAR_RANGE_PROPERTIES__

#include "smallBitvectors.h"
#include <algorithm>
#include <iostream>
#include <emmintrin.h>

//typedef uint8_t __attribute__((vector_size(16))) U8V;
typedef __m128i U8V;

//macros just so literally used at call-site, increasing chance of being totally inlined
#define AND_LAMBDA [] (U8 *x,U8 *y) -> U8 { return *x & *y;}
#define OR_LAMBDA [] (U8 *x,U8 *y) -> U8 { return *x | *y;}
#define MIN_LAMBDA [] (U8 *x,U8 *y) -> U8 { return std::min(*x,*y);}
#define MAX_LAMBDA [] (U8 *x,U8 *y) -> U8 { return std::max(*x,*y);}

#define AND_LAMBDAV [] (U8 *x,U8 *y) -> U8V { return _mm_loadu_si128((U8V*)x) & _mm_loadu_si128((U8V*)y);}
#define OR_LAMBDAV [] (U8 *x,U8 *y) -> U8V { return U8V();/**x | *y*/;}
#define MIN_LAMBDAV [] (U8 *x,U8 *y) -> U8V { return U8V();/*std::min(*x,*y)*/;}
#define MAX_LAMBDAV [] (U8 *x,U8 *y) -> U8V { return U8V();/*std::max(*x,*y)*/;}

#define IDX2(d,log2len,i) (((d)<<(log2len)) | (i))
extern int log2T[256];

/*indexes for a range property*/
struct SplitIdx {
    int i[2];
    SplitIdx(int log2len,int start,int end) { set(log2len,start,end); }
    void set(int log2len,int start,int end) {
        int depth=log2T[end-start];
        i[0]=IDX2(depth,log2len,start);
        i[1]=IDX2(depth,log2len,end-(1<<depth));
        std::cerr<<"SIdx "<<start<<" "<<end<<"->"<<depth<<" "<<i[0]<<" "<<i[1]<<"\n";
    }
};

void displayTable(U8* tbl,int len,int log2len);

void formFwdTable(U8 *elts,int len,int log2len,int depth);

void formBwdTable(U8 *elts,int len,int log2len,int depth);

void formUnionTable(U8 *elts,int len,int log2len,int depth);

void formIntersectTable(U8 *elts,int len,int log2len,int depth);

// ====== accessor functions ==============================
//scalar version
template<typename F>
inline U8 valueOverRange(U8* tbl,SplitIdx idx,F combiner) {
    std::cerr<<"ACC "<<idx.i[0]<<":"<<int(*(tbl+idx.i[0]))<<" "<<idx.i[1]<<":"<<int(*(tbl+idx.i[1]))
             <<"->"<<(int)(combiner(tbl+idx.i[0],tbl+idx.i[1]))<<"\n";
    return combiner(tbl+idx.i[0],tbl+idx.i[1]);
}

//vector version
template<typename F>
inline U8V valueOverRangeV(U8* tbl,SplitIdx idx,F combiner) {
    return combiner(_mm_loadu_si128((U8V*)(tbl+idx.i[0])) & _mm_loadu_si128((U8V*)(tbl+idx.i[0]));t
}

inline U8 andValueOverRange(U8 *tbl,SplitIdx idx) {
    return valueOverRange(tbl,idx,AND_LAMBDA);
}

inline U8 orValueOverRange(U8 *tbl,SplitIdx idx) {
    return valueOverRange(tbl,idx,OR_LAMBDA);
}

inline U8 maxValueOverRange(U8 *tbl,SplitIdx idx) {
    return valueOverRange(tbl,idx,MAX_LAMBDA);
}

inline U8 minValueOverRange(U8 *tbl,SplitIdx idx) {
    return valueOverRange(tbl,idx,MIN_LAMBDA);
}

inline bool equalRestrictRange(U8 *tblU,U8 *tblI,SplitIdx idx,U8 restriction) {
    return (orValueOverRange(tblU,idx) & restriction) == (andValueOverRange(tblI,idx) & restriction);
}
inline U8V andValueOverRangeV(U8 *tbl,SplitIdx idx) {
    return valueOverRangeV(tbl,idx,AND_LAMBDAV);
}

inline U8V orValueOverRangeV(U8 *tbl,SplitIdx idx) {
    return valueOverRangeV(tbl,idx,OR_LAMBDAV);
}

inline U8V maxValueOverRangeV(U8 *tbl,SplitIdx idx) {
    return valueOverRangeV(tbl,idx,MAX_LAMBDAV);
}

inline U8V minValueOverRangeV(U8 *tbl,SplitIdx idx) {
    return valueOverRangeV(tbl,idx,MIN_LAMBDAV);
}
#if 0
inline bool equalRestrictRange(U8 *tblU,U8 *tblI,SplitIdx idx,U8 restriction) {
    return (orValueOverRange(tblU,idx) & restriction) == (andValueOverRange(tblI,idx) & restriction);
}
#endif
// ============= range properties specific to the array compiler ==============
class RangeProperties {
public:
    static const int MAX_VARS=8;
    static const int VAR_UNION=0;
    static const int VAR_INTER=1;
    static const int GUARD_UNION=2;
    static const int GUARD_INTER=3;
    static const int REDUCTION_FWD=GUARD_INTER+1;
    static const int REDUCTION_BWD=REDUCTION_FWD+MAX_VARS;
    static const int NO_TBLS=REDUCTION_BWD+MAX_VARS;
#if 0
    static const int =;
    static const int =;
#endif
    void formTablesFromIndividuals();
    int len,depth,totalVars;
    U8* tables[NO_TBLS];
    U8 *idxsIndividual;
    U8 *guardsIndividual;
    U8 *reductionsIndividual;
};

#endif /*__LINEAR_RANGE_PROPERTIES__*/
