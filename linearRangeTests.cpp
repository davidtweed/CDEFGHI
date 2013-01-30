#include "linearRangeProperties.h"
#include <iostream>
#include <string.h>

typedef uint8_t (*CombineFn)(uint8_t *tbl,SplitIdx idx);
typedef uint8_t (*BasicCombineFn)(uint8_t*,uint8_t*);
typedef uint8_t Any;
void
generateRandomU8Table(uint8_t *tbl,int sz,int seed)
{
    int i;
    for(i=0;i<sz;++i){
        tbl[i]=(seed+i) % 256;
    }
}

int
exhaustiveTest(const char* const msg,uint8_t *tbl,int len,CombineFn fastFn,BasicCombineFn baseFn) {
    int log2len=int(log2T[len]);
    int sLen,i,j,k;
    int errorCount=0;
    for(sLen=1;sLen<3/*len*/;++sLen){
        for(i=0;i<len-sLen+1;++i){
            //do slow evaluation
            j=i+sLen;
            uint8_t v=tbl[i];
            for(k=i+1;k<j;++k){
                v=baseFn(static_cast<Any*>(&v),static_cast<Any*>(tbl+k));
            }
            SplitIdx idx(log2len,i,j);
            uint8_t v2=fastFn(tbl,idx);
            if(v!=v2){
                std::cerr<<msg<<": Fail "<<i<<" "<<j<<" "<<static_cast<int>(v)<<" "<<static_cast<int>(v2)<<"\n";
                ++errorCount;
            }
        }
    }
    return errorCount;
}

int
main(int argc,char* argv[]) {
    uint8_t tbl[256*8];
    memset(tbl,0,256*8*sizeof(char));
    int len=8/*256*/;
    int log2len=log2T[len];
    auto AND=AND_LAMBDA;
    int i;
    for(i=0;i<1;++i) {
        generateRandomU8Table(tbl,len,i);
        formUnionTable(tbl,len,log2len,log2len);
        //formIntersectTable(tbl,len,log2len,log2len);

        int j;
        for(j=0;j<32;++j){
            std::cerr<<int(tbl[j])<<"\n";
        }
        std::cerr<<std::flush;
        exit(0);
        int errorCount=exhaustiveTest("A",tbl,len,&andValueOverRange,AND);
        //errorCount+=exhaustiveTest("A",tbl,len,&andValueOverRange,AND);
    }
    return 0;
}
