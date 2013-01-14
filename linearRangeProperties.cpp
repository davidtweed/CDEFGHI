#include "linearRangeProperties.h"

using namespace std;
typedef std::pair<SBitvector,SBitvector> BVPr;

struct Copier {
    static void operator()(int depth,int pos,SBitvector a,SBitvector b,void *record) {
        SBitvector **rec=static_cast<SBitvector**>(record);
        rec[dep][2*pos+0]=a;
        rec[dep][2*pos+1]=b;
    }
    static void base(int pos,SBitvector a,void *record) {
        Byte **rec=static_cast<Rec**>(record);
        rec[0][2*pos+0]=a;
        rec[0][2*pos+1]=a;
    }
};

struct Same {
    static void operator()(int depth,int pos,SBitvector a,SBitvector b,void *record) {
        Byte **rec=static_cast<Rec**>(record);
        rec[dep][pos]=static_cast<Byte>(a==b);
    }
    static void base(int pos,SBitvector a,void *record) {
        Byte **rec=static_cast<Rec**>(record);
        rec[0][pos]=static_cast<Byte>(true);
    }
};

struct Consistent {
    static void operator()(int depth,int pos,SBitvector a,SBitvector b,void *record) {
        Byte **rec=static_cast<Rec**>(record);
        rec[dep][pos]=static_cast<Byte>(a==b);
    }
    static void base(int pos,SBitvector a,void *record) {
        Byte **rec=static_cast<Rec**>(record);
        rec[0][pos]=static_cast<Byte>(a);
    }
};



template<class F>
inline void f (int noElts,void *record,const F &fn) {
    vector<BVPr> stk;
    while(1) {
        un=cup(l,r);
        in=cap(l,r);
        fn(depth,pos,un,in,record);
        stk.push_back(BVPr(un,in));
        stk.pop();
        stk.pop();
    }
}

/*
 * assumes first row filled in
 * TODO: deal with SIMD issues
 */
template<typename T,F>
inline
void formTable(T** elts,int len,int depth,F combiner) {
    int delta=1;
    int i=1;
    do{
        for(j=0;j<len-delta;++j){
            elts[i][j]=combiner(elts[i-1][j],elts[i-1][j+delta]);
        }
        ++i;
        delta *= 2;
    }while(i<depth);
}

void formFwdTable(uint8_t** elts,int len,int depth) {
    formTable(elts,len,[] (uint8_t x,uint8_t y) { return std::min(x,y);});
}

void formBwdTable(uint8_t** elts,int len,int depth) {
    formTable(elts,len,[] (uint8_t x,uint8_t y) { return std::max(x,y);});
}

void formUnionTable(uint8_t** elts,int len,int depth) {
    formTable(elts,len,[] (uint8_t x,uint8_t y) { return x | y;});
}

void formIntersectTable(uint8_t** elts,int len,int depth) {
    formTable(elts,len,[] (uint8_t x,uint8_t y) { return x & y;});
}

