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

template<typename T,int FIN_SZ>
inline T rangeLookup(T** tbl,int start,int end) {
    return T[start][FIN_SZ*start];
}
