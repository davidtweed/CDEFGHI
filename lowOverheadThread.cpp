#include "lowOverheadThread"
#include <assert>

LinearAlloc::LinearAlloc(Byte *sbase,int sbytesLen,int sblkSz)
{
    assert(intptr_t(sbase)%sblkSz==0);
    assert(bytesLen%sblkSz==0);
    base=sbase;
    bytesLen=sbytesLen;
    limit=base+bytesLen;
    curPos=base;
    blkSz=sblkSz;
    startBlkAlloc();
}

LinearAlloc::~LinearAlloc()
{
/*do nothing*/
}

void* LinearAlloc::getBlock(size_t szBytes) {
    void *p=curPos;
    szBytes=((szBytes+blkSz-1)/blkSz)*blkSz;
    curPos+=szBytes;
    if(curPos>limit){
        //error
    }
    return p;
}


void
spawnChildren()
{
    noFinished=0;
    int i;
    for(i=1;i<=noChildren;++i){
        if(fork()!=-1){
            runChildThread(i);
        }
    }
}

void
ThreadController::doWork(int ltid)
{
    (*fp)(commonWork,individualWorks[ltid]);
}

void
ThreadController::runChildThread(int ltid)
{
    while(true){
        int noFinishedVal;
        while((noFinishedVal=noFinished)!=noChildren){
            futex_sleep(&noFinished,noFinishedVal); //avoid missed-wakeup race
        }
        doWork(ltid);
        atomic_dec(noFinished);
    }
    _exit(0);
}

void
ThreadController::runWithChildren()
{
    noFinished=noChildren;
    futex_wake(&noFinished,INT_MAX);
    doWork(0); //do a slice of the work in main thread
    while(noFinished!=0){
        //spin until all threads have finished
    }
}
