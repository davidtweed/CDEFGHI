#include "linearRangeProperties.h"
#include <algorithm>
#include <iostream>
#include <assert.h>

using namespace std;

//floor(log2(x)), ie, v such that (1<<v) <= x && (1<<(v+1))>x (x must be > 0)
int log2T[256]={0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7};

//NOTE: one way of determining if bitsets A_i == A_{i+1} == ... == A_j is to test if
//union(A_i,...,A_j) == intersection(A_i,...,A_j). This is useful because unions can
//be precomputed whereas direct equality tests can't (quite).

void displayTable(U8* tbl,int len,int log2len) {
    int d,i;
    for(d=0;d<log2T[len];++d){
        std::cerr<<"D"<<d<<"    ";
        for(i=0;i<len;++i){
            std::cerr<<i<<":"<<static_cast<int>(tbl[IDX2(d,log2len,i)])<<" ";
        }
        std::cerr<<"\n";
    }
    std::cerr<<"\n";
}

// ========== building tables ===================

/*
 * assumes first row filled in
 * TODO: deal with SIMD issues
 */

template<typename T,typename F>
inline
void preprocessIndividuals(T* tblOut,T* tblIn,int len,F preproc) {
    int i=0;
    do{
        tblOut[i]=preproc(tblIn[i]);
    }while(++i<len);
}

template<typename T,typename F>
inline
void formTable(T* elts,int len,int log2len,int depth,F combiner) {
    int delta=1;
    int i=1;
    do{
        int idx0=IDX2(i,log2len,0);
        int lim=idx0 | (len-2*delta+1);
        int idx1=IDX2(i-1,log2len,0);
        int idx2=idx1 | delta;
        int j=0;//assert(1==0);
        do{
            elts[idx0]=combiner(elts+idx1,elts+idx2);
            ++idx0;
            ++idx1;
            ++idx2;
            ++j;
        }while(idx0<lim);
        ++i;
        delta *= 2;
    }while(i<depth);
}

template<typename T,typename F1,typename F2>
inline
void formTable2(T* elts1,T* elts2,int len,int log2len,int depth,F1 combiner1,F2 combiner2) {
    int delta=1;
    int i=1;
    do{
        int idx0=IDX2(i,log2len,0);
        int lim=idx0 | (len-delta);
        int idx1=IDX2(i-1,log2len,0);
        int idx2=idx1 | delta;
        do{
            elts1[idx0]=combiner1(elts1+idx1,elts2+idx2);
            elts2[idx0]=combiner2(elts1+idx1,elts2+idx2);
            ++idx0;
            ++idx1;
            ++idx2;
        }while(idx0<lim);
        ++i;
        delta *= 2;
    }while(i<depth);
}

void intersectNonempty(U8* tblOut,U8* tblIn,int len,int log2len,int depth) {
    preprocessIndividuals(tblOut,tblIn,len,[] (U8 a) { return a==0?0xFF:a;  } );
    formTable(tblOut,len,log2len,depth,AND_LAMBDA);
}

/*inline*/ void formFwdTable(U8* elts,int len,int log2len,int depth) {
    formTable(elts,len,depth,log2len,MIN_LAMBDA);
}


/*inline*/ void formBwdTable(U8* elts,int len,int log2len,int depth) {
    formTable(elts,len,depth,log2len,MAX_LAMBDA);
}

/*inline*/ void formUnionTable(U8* elts,int len,int log2len,int depth) {
    formTable(elts,len,depth,log2len,OR_LAMBDA);
}

/*inline*/ void formIntersectTable(U8* elts,int len,int log2len,int depth) {
    formTable(elts,len,depth,log2len,AND_LAMBDA);
}

void formUITable(U8* eltsU,U8* eltsI,int len,int log2len,int depth) {
    formTable2(eltsU,eltsI,len,depth,log2len,OR_LAMBDA,AND_LAMBDA);
}

//a
void RangeProperties::formTablesFromIndividuals() {
    int log2len;
    formUITable(tables[VAR_UNION],tables[VAR_INTER],len,log2len,depth);
    formUITable(tables[GUARD_UNION],tables[GUARD_INTER],len,log2len,depth);
#if 0
    //formIntersectTable(tables[],len,depth);
    int i;
    for(i=0;i<totalVars;++i){
        formFwdTable(tables[REDUCTION_FWD+i],len,log2len,depth);
        formBwdTable(tables[REDUCTION_BWD+i],len,log2len,depth);
    }
#endif
}



void checkValid() {


    //check guards conditions
    //g();
}
