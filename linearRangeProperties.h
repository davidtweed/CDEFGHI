#ifndef __LINEAR_RANGE_PROPERTIES__
#define __LINEAR_RANGE_PROPERTIES__

#include "smallBitvectors.h"
#include <algorithm>

//macros just so literally used at call-site, increasing chance of being totally inlined
#define AND_LAMBDA [] (uint8_t x,uint8_t y) { return x & y;}
#define OR_LAMBDA [] (uint8_t x,uint8_t y) { return x | y;}
#define MIN_LAMBDA [] (uint8_t x,uint8_t y) { return std::min(x,y);}
#define MAX_LAMBDA [] (uint8_t x,uint8_t y) { return std::max(x,y);}

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
    }
};

void formFwdTable(uint8_t *elts,int len,int log2len,int depth);

void formBwdTable(uint8_t *elts,int len,int log2len,int depth);

void formUnionTable(uint8_t *elts,int len,int log2len,int depth);

void formIntersectTable(uint8_t *elts,int len,int log2len,int depth);

// ====== accessor functions ==============================
template<typename T,typename F>
inline T valueOverRange(T* tbl,SplitIdx idx,F combiner) {
    return combiner(tbl[idx.i[0]],tbl[idx.i[1]]);
}

inline uint8_t andValueOverRange(uint8_t *tbl,SplitIdx idx) {
    return valueOverRange(tbl,idx,AND_LAMBDA);
}

inline uint8_t orValueOverRange(uint8_t *tbl,SplitIdx idx) {
    return valueOverRange(tbl,idx,OR_LAMBDA);
}

inline uint8_t maxValueOverRange(uint8_t *tbl,SplitIdx idx) {
    return valueOverRange(tbl,idx,MAX_LAMBDA);
}

inline uint8_t minValueOverRange(uint8_t *tbl,SplitIdx idx) {
    return valueOverRange(tbl,idx,MIN_LAMBDA);
}

inline bool equalRestrictRange(uint8_t *tblU,uint8_t *tblI,SplitIdx idx,uint8_t restriction) {
    return (orValueOverRange(tblU,idx) & restriction) == (andValueOverRange(tblI,idx) & restriction);
}

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
    uint8_t* tables[NO_TBLS];
    uint8_t *idxsIndividual;
    uint8_t *guardsIndividual;
    uint8_t *reductionsIndividual;
};

#endif /*__LINEAR_RANGE_PROPERTIES__*/
