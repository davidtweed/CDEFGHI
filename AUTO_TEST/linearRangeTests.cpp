#include "../linearRangeProperties.h"
#include <iostream>
#include <string.h>

typedef U8 (*CombineFn)(U8 *tbl,SplitIdx idx);
typedef U8 (*BasicCombineFn)(U8*,U8*);
typedef U8 Any;
void
generateRandomU8Table(U8 *tbl,int sz,int seed)
{
    int i;
    for(i=0;i<sz;++i){
        tbl[i]=(seed+i) % 256;
    }
}

int
exhaustiveTest(const char* const msg,U8 *tbl,int len,CombineFn fastFn,BasicCombineFn baseFn) {
    int log2len=int(log2T[len]);
    int sLen,i,j,k;
    int errorCount=0,tried=0;
    std::cerr<<"Error count "<<errorCount<<"\n";
    for(sLen=1;sLen<len;++sLen){
        for(i=0;i<len-sLen+1;++i){
            ++tried;
            //do slow evaluation
            j=i+sLen;
            U8 v=tbl[i];
            for(k=i+1;k<j;++k){
                v=baseFn(static_cast<Any*>(&v),static_cast<Any*>(tbl+k));
            }
            SplitIdx idx(log2len,i,j);
            U8 v2=fastFn(tbl,idx);
            if(v!=v2){
                std::cerr<<msg<<": Fail "<<i<<" "<<j<<": "<<static_cast<int>(v)<<" got "<<static_cast<int>(v2)<<"\n";
                ++errorCount;
            }else{std::cerr<<msg<<": OK "<<i<<" "<<j<<": "<<static_cast<int>(v)<<" == "<<static_cast<int>(v2)<<"\n";
            }
        }
    }
    std::cerr<<"Error count "<<errorCount<<" in "<<tried<<"\n";
    return errorCount;
}

#define DOIT(m,b,f,l) b(tbl,len,log2len,log2len);   \
/*displayTable(tbl,len,log2len);*/ \
errorCount+=exhaustiveTest(m,tbl,len,f,l)

int
main(int argc,char* argv[]) {
    U8 tbl[256*8];
    memset(tbl,0,256*8*sizeof(char));
    int len=16/*256*/;
    int log2len=log2T[len];
    auto AND=AND_LAMBDA;
    auto OR=OR_LAMBDA;
    auto MIN=MIN_LAMBDA;
    auto MAX=MAX_LAMBDA;
    //test the scalar code
    int i;
    for(i=0;i<100;++i) {
        int errorCount=0;
        generateRandomU8Table(tbl,len,i);
        DOIT("V",formUnionTable,&orValueOverRange,OR);
        DOIT("^",formIntersectTable,&andValueOverRange,AND);
        DOIT("min",formFwdTable,&minValueOverRange,MIN);
        DOIT("max",formBwdTable,&maxValueOverRange,MAX);
        //errorCount+=exhaustiveTest("A",tbl,len,&andValueOverRange,AND);
        std::cout<<"Total errors "<<errorCount<<"\n";
    }
    return 0;
}
