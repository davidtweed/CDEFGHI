#ifndef __LOW_OVERHEAD_THREAD_H__
#define __LOW_OVERHEAD_THREAD_H__

#include <stack>

typedef volatile int32_t BarrierVar;

void
createStructInSharedMemory()
{

}
//VITAL: structure must be of constant size no matter what it holds so can "construct ptr to it" before fork()
struct ArrayList {
    int noArrays;
    int noIdxs;
    uint64_t validMask;
    void** arrays;
};

//VITAL: structure must be of constant size no matter what it holds so can "construct ptr to it" before fork()
struct ArrayIterSpec {

};

//in order to get correct alignment we "remove" chunks in mulitples of blkSz
//even if less is requested
class LinearAlloc {
public:
    typedef char Byte;
    void* getBlock(size_t szBytes);
    void reset() { curPos=resetPts.top(); resetPts.pop(); }
    void startBlkAlloc() { resetPts.push(curPos); }
    LinearAlloc(Byte *sbase,int sbytesLen,int sblkSz);
    ~LinearAlloc(); //not our memory
    Byte *base,*limit;
    Byte *curPos;
    stack<Byte*> resetPts;
    int bytesLen; //length of regions
    int blkSz;
};

class ThreadController {
public:
    static const int MAX_THREADS=8;
    void runChildThread(int cid);
    void runWithChildren();
    void doWork(int ltid);
    BarrierVar noFinished; //atomic variables
    int noChildren;
    void *commonWork; //points into shared memory
    void *individualWorks[MAX_THREADS]; //points into shared memory
    LinearAlloc &memory; // controller containing shared memory
    void (*fp)(void *cw,void *iw);
    int *childWakeVars; //atomic variables
    bool quit;
};

#endif /*__LOW_OVERHEAD_THREAD_H__*/
