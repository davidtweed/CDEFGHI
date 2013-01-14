#include "linearRangeProperties.h"
#include <algorithm>

using namespace std;
typedef std::pair<SBitvector,SBitvector> BVPr;

int log2T[256]={0,0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7};

//NOTE: one way of determining if bitsets A_i == A_{i+1} == ... == A_j is to test if
//union(A_i,...,A_j) == intersection(A_i,...,A_j). This is useful because unions can
//be precomputed whereas direct equality tests can't (quite).
/*
 * assumes first row filled in
 * TODO: deal with SIMD issues
 */

template<typename T,typename F>
inline
void formTable(T** elts,int len,int l2l,int depth,F combiner) {
    int delta=1;
    int i=1;
    do{
        for(int j=0;j<len-delta;++j){
            elts[i][j]=combiner(elts[i-1][j],elts[i-1][j+delta]);
        }
        ++i;
        delta *= 2;
    }while(i<depth);
}

template<typename T,typename F1,typename F2>
inline
void formTable2(T* elts1,T* elts2,int len,int l2l,int depth,F1 combiner1,F2 combiner2) {
    int delta=1;
    int i=1;
    do{
        int idx0=IDX2(i,l2l,0);
        int lim=idx0 | (len-delta);
        int idx1=IDX2(i-1,l2l,0);
        int idx2=idx1 | delta;
        do{
            elts1[idx0]=combiner1(elts1[idx1],elts2[idx2]);
            elts2[idx0]=combiner2(elts1[idx1],elts2[idx2]);
            ++idx0;
            ++idx1;
            ++idx2;
        }while(idx0<lim);
        ++i;
        delta *= 2;
    }while(i<depth);
}
#if 0
inline void formFwdTable(uint8_t** elts,int len,int l2l,int depth) {
    formTable(elts,len,depth,l2l,MIN_LAMBDA);
}


inline void formBwdTable(uint8_t** elts,int len,int l2l,int depth) {
    formTable(elts,len,depth,l2l,MAX_LAMBDA);
}

inline void formUnionTable(uint8_t** elts,int len,int l2l,int depth) {
    formTable(elts,len,depth,l2l,OR_LAMBDA);
}

inline void formIntersectTable(uint8_t** elts,int len,int l2l,int depth) {
    formTable(elts,len,depth,l2l,AND_LAMBDA);
}
#endif
void formUITable(uint8_t* eltsU,uint8_t* eltsI,int len,int l2l,int depth) {
    formTable2(eltsU,eltsI,len,depth,l2l,OR_LAMBDA,AND_LAMBDA);
}


class RangeTables {
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

//a
void RangeTables::formTablesFromIndividuals() {
    int l2l;
    formUITable(tables[VAR_UNION],tables[VAR_INTER],len,l2l,depth);
    formUITable(tables[GUARD_UNION],tables[GUARD_INTER],len,l2l,depth);
#if 0
    //formIntersectTable(tables[],len,depth);
    int i;
    for(i=0;i<totalVars;++i){
        formFwdTable(tables[REDUCTION_FWD+i],len,l2l,depth);
        formBwdTable(tables[REDUCTION_BWD+i],len,l2l,depth);
    }
#endif
}

template<typename T,typename F>
inline T valueOverRange(T* tbl,int l2l,int start,int end,F combiner) {
    int depth=log2T[end-start];
    return combiner(tbl[IDX2(depth,l2l,start)],tbl[IDX2(depth,l2l,end-(1<<depth))]);
}

template<typename T,typename F1,typename F2>
inline std::pair<T,T> valueOverRange2(T* tbl1,T* tbl2,int l2l,int start,int end,F1 combiner1,F2 combiner2) {
    int depth=log2T[end-start];
    int idxL=IDX2(depth,l2l,start),idxR=IDX2(depth,l2l,end-(1<<depth));
    return std::make_pair(combiner1(tbl1[idxL],tbl1[idxR]),combiner2(tbl2[idxL],tbl2[idxR]));
}

bool equalRestrictRange(uint8_t *tblU,uint8_t *tblI,int l2l,uint8_t restriction,int start,int end) {
    std::pair<uint8_t,uint8_t> v=valueOverRange2(tblU,tblI,l2l,start,end,OR_LAMBDA,AND_LAMBDA);
    return (v.first & restriction) == (v.second & restriction);
}

uint8_t andValueOverRange(uint8_t *tbl,int l2l,int start,int end) {
    return valueOverRange(tbl,l2l,start,end,AND_LAMBDA);
}

uint8_t orValueOverRange(uint8_t *tbl,int l2l,int start,int end) {
    return valueOverRange(tbl,l2l,start,end,OR_LAMBDA);
}

uint8_t maxValueOverRange(uint8_t *tbl,int l2l,int start,int end) {
    return valueOverRange(tbl,l2l,start,end,MAX_LAMBDA);
}

uint8_t minValueOverRange(uint8_t *tbl,int l2l,int start,int end) {
    return valueOverRange(tbl,l2l,start,end,MIN_LAMBDA);
}
