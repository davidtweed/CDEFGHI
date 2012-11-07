#ifndef ARRAY_ELISION_BUILDER_H
#define ARRAY_ELISION_BUILDER_H

#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/JITEventListener.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Intrinsics.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Analysis/DebugInfo.h"
#include "llvm/Analysis/DIBuilder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/TargetSelect.h"
#include <vector>
#include <stdlib.h>
#include <stdint.h>
#define null_ptr 0
using namespace std;
using namespace llvm;

typedef uint32_t Bitmask;
#ifdef SYS_64BIT
typedef int64_t Index;
#else
typedef int32_t Index;
#endif
struct TraversalRange;

/*SUBTLE: we're dealing with a DAG, so recursive algorithms need to check
 *if already visited a node. In Exp done by having llvmTemp set non-zero
 */
class LLCompilerState {
public:
    LLVMContext *TheLLVMContext;// = getGlobalContext();
    IRBuilder<> *builder;//(getGlobalContext());
    bool nested_compile=false;
    Module *TheModule;
    ExecutionEngine *TheExecutionEngine;
    DIBuilder *dbuilder;
    FunctionPassManager *FPM;
    static const int MAX_RANGES=5;
    TraversalRange* traversalRanges;
    bool enableLowlevelOpt;
    bool verbose;
    bool addTiming; //NO overflow protection, so small loops only
    Value* curIdxVars[MAX_RANGES];
};

typedef uint8_t Operation;
const Operation NullOp=0xFF;
typedef uint8_t EltType;
#if 0
//give the operation & its "neutral" value
struct OpRec {
public:
    OpWrapperBase *fp;
    Value *neutral;
};
#endif
const int MAX_TOT_DIMS=256;
const int MAX_ARR_DIMS=3;

//difference between sizes and allocd can arise from "where" clauses
//probably difficult to alloc more than 2^32 floats on desktop machine, but maybe with mmap on an SSD...
struct ArrRec {
    Value *basePtr;
    char *name;
    uint8_t origSzIdxs[MAX_ARR_DIMS];
    uint8_t sizes[MAX_ARR_DIMS];
    uint8_t allocd[MAX_ARR_DIMS];
    EltType type;
    uint8_t noDims;
    uint8_t needsMaterialisation;
};
//extent[0] is always a special zero value
struct ProblemState {
    Index extents[MAX_TOT_DIMS];
    int fstUnusedExtent;
    int noArrays;
    ArrRec* arrays;
};
struct DisplayRec {
    ProblemState *ps;
    std::ostream *s;
};
//loops are actually bivalued, with input:output indices
//the
class Exp;
class VarRec;
//typedef Exp* ExpPtr;
typedef std::shared_ptr<Exp> ExpPtr;
//delivers an "insert index,lookup index pair", or null if exhausted
/*class Where {
public:
    Value *inIdx,*outIdx;
    ExpPtr cond; //can validly be null
    };*/
struct TraversalRange {
    //ranges are {iterStart,iterStop,dimensionMax}
    Index bnds[3];
    ExpPtr *where; //actual expression used in where clause
    Value *llvmValues[2]; //scalar index that holds value containing idx
    //0-OUT, 1-IN
};
//return of null_ptr-> no match, otherwise the child
typedef Exp* (*EPred)(Exp* e,void* opaque);

class Exp {
public:
    typedef void (Exp::*VFn)(void*); //pointer to member function
    Exp(uint8_t snodeType,EltType stype);
    virtual ~Exp();
    virtual Value* generateSpecific(LLCompilerState& global) = 0;
    virtual void wipeSpecific(void*) = 0;
    //precondition: tgtLoopBodyIdx>=loopBodyIdx
    virtual Value* outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,uint8_t tgtLoopBodyIdx)=0;
    //maybe use in pattern matching simplification later
    virtual Exp* childSatisfies(EPred pred,void* opaque)=0;
    virtual void display(void* opaque)=0;
    virtual void recVisitor(VFn vfn,void* opaque)=0;
    void recVisitorBase(VFn vfn,void* opaque);
    //precondition: tgtLoopBodyIdx>=loopBodyIdx-1
    Value* outputNodeIf(LLCompilerState &gs,ProblemState &ps,uint8_t tgtLoopBodyIdx);
    void wipeLLVM(void*);
    void displayBase(void *opaque);
    Value* generateCode(LLCompilerState& global);
    VarRec *output;
    Value *llvmTemp;
    Bitmask idxsInKids; //bitmask of indexes used in child nodes
    EltType type; //unless subclass specisfies real in-type == out-type
    uint8_t loopBodyIdx;
    uint8_t nodeType;
};
class Collection : public Exp {
public:
    Collection(vector<ExpPtr> &selts);
    ~Collection();
    Exp* childSatisfies(EPred pred,void* opaque);
    void recVisitor(VFn vfn,void* opaque);
    Value* generateSpecific(LLCompilerState& global);
    void wipeSpecific(void*);
    Value* outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,uint8_t tgtLoopBodyIdx);
    void display(void* opaque);
    std::vector<ExpPtr> elts;
    static const int TYPECODE=0;
};
class NumLiteral : public Exp {
public:
    NumLiteral(int64_t v);
    NumLiteral(uint64_t v);
    NumLiteral(double v);
    ~NumLiteral();
    void recVisitor(VFn vfn,void* opaque);
    Exp* childSatisfies(EPred pred,void* opaque);
    Value* generateSpecific(LLCompilerState& global);
    void wipeSpecific(void*);
    Value* outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,uint8_t tgtLoopBodyIdx);
    void display(void* opaque);
    union {
        double fMem;
        int64_t sMem;
        uint64_t uMem;
    };
    static const int TYPECODE=1;
};
class VarRec : public Exp {
public:
    VarRec(Value* sllvmTemp,EltType stype); //used for creating dummy objects
    VarRec(ArrRec *a,EltType stype);
    ~VarRec();
    void recVisitor(VFn vfn,void* opaque);
    Exp* childSatisfies(EPred pred,void* opaque);
    Value* generateSpecific(LLCompilerState& global);
    void wipeSpecific(void*);
    Value* outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,uint8_t tgtLoopBodyIdx);
    void display(void* opaque);
    ArrRec *r;
    uint8_t idxs[MAX_ARR_DIMS];
    int8_t deltas[MAX_ARR_DIMS];
    int8_t noIdxs;
    static const int TYPECODE=2;
};
const int MAX_GEN_STATS=4;
class RandArr : public Exp {
public:
    RandArr(EltType stype,uint8_t srandType);
    ~RandArr();
    void recVisitor(VFn vfn,void* opaque);
    Exp* childSatisfies(EPred pred,void* opaque);
    Value* generateSpecific(LLCompilerState& global);
    void wipeSpecific(void*);
    Value* outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,uint8_t tgtLoopBodyIdx);
    void display(void* opaque);
    Value *suffStatsD[MAX_GEN_STATS];
    uint8_t randType;
    static const int TYPECODE=3;
};
//for future use where a julia/C call-back function is used
class UFnApp : public Exp {
public:
    UFnApp(ExpPtr sinput,void *sfnPtr,EltType stype);
    ~UFnApp();
    void recVisitor(VFn vfn,void* opaque);
    Value* generateSpecific(LLCompilerState& global);
    void wipeSpecific(void*);
    Value* outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,uint8_t tgtLoopBodyIdx);
    void display(void* opaque);
    Exp* childSatisfies(EPred pred,void* opaque);
    ExpPtr input;
    void *fnPtr;
    EltType inT,outT;
    static const int TYPECODE=4;
};
struct Combiner : public Exp {
    Combiner(Operation soperation,EltType stype,ExpPtr i0,ExpPtr i1=0,ExpPtr i2=0);
    ~Combiner();
    void recVisitor(VFn vfn,void* opaque);
    Value* generateSpecific(LLCompilerState& global);
    void wipeSpecific(void*);
    Value* outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,uint8_t tgtLoopBodyIdx);
    void display(void* opaque);
    Exp* childSatisfies(EPred pred,void* opaque);
    ExpPtr inputs[3];
    Operation operation;
    EltType evalType;
    bool reorderable;
    static const int TYPECODE=5;
};
/*
 *It's VITAL that the variable inputs to an accumulation have been evaluated in
 *the body (as otherwise recursion may create more basic blocks, rendering
 *phi nodes invalid) so that they can be computed directly.
 */
class Traversal : public  Exp {
public:
    Traversal(Collection &sbody,Collection &saccumulations,EltType stype);
    ~Traversal();
    void recVisitor(VFn vfn,void* opaque);
    Value* generateSpecific(LLCompilerState& global);
    void wipeSpecific(void*);
    Value* outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,uint8_t tgtLoopBodyIdx);
    void display(void* opaque);
    Exp* childSatisfies(EPred pred,void* opaque);
    Collection body;
    Collection accumulations;
    uint8_t noIdxs;
    uint8_t idxs[3];
    static const int TYPECODE=6;
};
#define OP_DATA XX("abs",0,ABS,1)                    \
    XX("sqrt",1,SQRT,1)\
    XX("exp",2,EXP,1)\
    XX("log",3,LOG,1)\
    XX("floor",4,FLOOR,1)\
    XX("sin",5,SIN,1)\
    XX("cos",6,COS,1)\
    XX("not",7,NOT,1)\
    XX("+",8,ADD,2)\
    XX("=",9,SUBTRACT,2)\
    XX("*",10,MULTIPLY,2)                           \
    XX("/",11,DIVIDE,2)\
    XX("^",12,NPOWER,2)\
    XX("^^",13,POWER,2)\
    XX("==",14,EQ,2)\
    XX("<",15,LT,2)\
    XX(">",16,GT,2)\
    XX("<=",17,LE,2)\
    XX(">=",18,GE,2)\
    XX("&",19,AND,2)\
    XX("&~",20,ANDNOT,2)\
    XX("|",21,OR,2)\
    XX("!$",22,XOR,2)\
    XX("&&",23,BAND,2)\
    XX("&&~",24,BANDNOT,2)\
    XX("||",25,BOR,2)\
    XX("!$!$",26,BXOR,2)\
    XX("\\/",27,MAX,2)\
    XX("/\\",28,MIN,2)\
    XX("@@",29,WHERE,2)\
    XX("select",30,SELECT,3)\
    XX("fma",31,FMA,3)

//declare the opcodes
#define XX(s,a,b,c) const Operation b=a;
OP_DATA;
#undef XX

#endif /*ARRAY_ELISION_BUILDER_H*/
