#include "arrayElisionBuilder.h"
#include <iostream>

#define STRINGIFY(x) #x
#define panic_if(x,y,v) if(x) { std::cerr<<(STRINGIFY(__LINE__) y)<<STRINGIFY(v)<<"="<<v<<std::endl<<std::flush;abort(); }

#define BLK(v,n) const EltType v=n<<SHIFT_AMT

//1-bit bools needed?
#define SHIFT_AMT 0
BLK(W8,1);BLK(W16,2);BLK(W32,3);BLK(W64,4);BLK(W128,5);
#undef SHIFT_AMT

#define SHIFT_AMT 6
BLK(SINT,0);BLK(UINT,1);BLK(FLOAT,2);BLK(BOOL,3);
#undef SHIFT_AMT

#define SHIFT_AMT 3
BLK(SCALAR1,0);BLK(VECTOR2,1);BLK(VECTOR4,2);BLK(VECTOR8,3);BLK(VECTOR16,4);BLK(VECTOR32,5);
#undef SHIFT_AMT

//"SWITCH-OP" macros
//"X-macros" kind of thing
#define SO2(x,t1,f1,t2,f2) SO(x,t1,f1);SO(x,t2,f2)
#define SO3(x,t1,f1,t2,f2,t3,f3) SO(x,t1,f1);SO(x,t2,f2);SO(x,t3,f3)
//switch-op-default type ordering
#define SOD3(x,f1,f2,f3) SO3(x,UINT,f1,SINT,f2,FLOAT,f3)

bool
newValueInIdxSet(Bitmask oldValue,Bitmask newValue) {
    return (newValue & (~oldValue))!=0;
}

Value* CST(int n) { return ConstantInt::get(getGlobalContext(),APInt(32,n)); }

inline bool isFloating(EltType type) { return FLOAT & type; }

//TODO: fix corner cases like min/max limits
Value* mkConst(EltType type,int64_t n) {
    if(isFloating(type)){ //floating point
        return ConstantInt::get(getGlobalContext(),APInt(8*(type&7),n));
    }else{ //discrete variable
        int noBits=8*(type&7);
        int64_t mask=(noBits==64?0xFFFFFFFF:(1<<noBits)-1);
        return ConstantInt::get(getGlobalContext(),APInt(noBits,mask&n));
    }
}

#define XX(s,a,b,c) c,
const int opArity[]={OP_DATA 0};
#undef XX

#define XX(s,a,b,c) #b,
const char* const opNames[]={OP_DATA ""};
#undef XX

Type* typeDB[4][6]={{0,0,0,0,0,0},{0,0,0,0,0,0},{0,0,0,0,0,0},{0,0,0,0,0,0}};

constexpr int fundamentalTypeCode(int x) { return (x>>6)&3; }

#define BP(x,a,b) XX(x,a,b)

void initialSetup() {
/*
   #define SS(t,s,l) typeDB[fundamentalTypeCode(t)][s]=Type::l(getGlobalContext());
   BP(UINT,W8,getInt8Ty,W16,getInt16Ty,W32,getInt32Ty,W64,getInt64Ty);
   BP(SINT,W8,getInt8Ty,W16,getInt16Ty,W32,getInt32Ty,W64,getInt64Ty);
   BP(FLOAT,W32,getFloatTy,W64,getDoubleTy);*/
    typeDB[fundamentalTypeCode(UINT)][W8]=Type::getInt8Ty(getGlobalContext());
    typeDB[fundamentalTypeCode(UINT)][W16]=Type::getInt16Ty(getGlobalContext());
    typeDB[fundamentalTypeCode(UINT)][W32]=Type::getInt32Ty(getGlobalContext());
    typeDB[fundamentalTypeCode(UINT)][W64]=Type::getInt64Ty(getGlobalContext());
    typeDB[fundamentalTypeCode(SINT)][W8]=Type::getInt8Ty(getGlobalContext());
    typeDB[fundamentalTypeCode(SINT)][W16]=Type::getInt16Ty(getGlobalContext());
    typeDB[fundamentalTypeCode(SINT)][W32]=Type::getInt32Ty(getGlobalContext());
    typeDB[fundamentalTypeCode(SINT)][W64]=Type::getInt64Ty(getGlobalContext());
    typeDB[fundamentalTypeCode(FLOAT)][W32]=Type::getFloatTy(getGlobalContext());
    typeDB[fundamentalTypeCode(FLOAT)][W64]=Type::getDoubleTy(getGlobalContext());
}

Type* llvmType(EltType e) {
    return typeDB[fundamentalTypeCode(e)][e&7];
}


//matches formulae used in ref() in array.jl
int idxsToLinear(LLCompilerState& gs,ArrRec *arrRec,int *idxs) {

    int v=idxs[arrRec->noDims-1];
    int i;
    for(i=arrRec->noDims-2;i>=0;--i){
        /*
          v=v*gs.traversalRanges[arrRec->origSzIdxs[i+1]][3]+idxs[i];*/
    }
    return v;
}

template<typename U,typename T>
class TinyMap {
public:
    U keys[8];
    T vals[8];
    int sz;
    T* lookup(U &k) {
        int i;
        for(i=0;i<sz;++i){
            if(keys[i]==k){
                return &(vals[i]);
            }
        }
        return null_ptr;
    }
};

Value* getIdxValue(LLCompilerState& gs,ProblemState &ps,int arrNo,TinyMap<int8_t,Value*> &idxs,int idxNo) {
    Value **id=idxs.lookup(ps.arrays->idxToExtent[idxNo]);
    assert(id!=0 && "index not found");
    Value *v=*id;
    if(0){//add on delta

    }
    return v;
}

//produce integer indexing into an array with constant dimension but Value* indices
Value* idxsToLinearSymbolic(LLCompilerState& gs,ProblemState &ps,int arrNo,TinyMap<int8_t,Value*> &idxs) {
    Value* v=CST(0);
    if(ps.arrays->noDims!=0){
        Value *v=getIdxValue(gs,ps,arrNo,idxs,0);
        Value *mul=CST(1);
        int i;
        for(i=1;i<ps.arrays->noDims;++i){
            mul=gs.builder->CreateMul(mul,CST(ps.arrays->idxToExtent[i]));
            Value *id=getIdxValue(gs,ps,arrNo,idxs,i);
            v=gs.builder->CreateAdd(v,gs.builder->CreateMul(id,mul));
        }
    }
    return v;
}

Value* createGEP(LLCompilerState& gs,EltType t,ProblemState &ps,int arrNo,TinyMap<int8_t,Value*> &idxs) {
    Value* idxVar=idxsToLinearSymbolic(gs,ps,arrNo,idxs);
    Value* indexes[2]={CST(0),idxVar};
    return gs.builder->CreateGEP(ps.arrays->basePtr,ArrayRef<Value*>(indexes));
}

Value* loadValueFromArraySlot(LLCompilerState& gs,EltType t,ProblemState &ps,int arrNo,TinyMap<int8_t,Value*> &idxs) {
    return gs.builder->CreateLoad(createGEP(gs,t,ps,arrNo,idxs));
}

Value* storeValueToArraySlot(LLCompilerState& gs,EltType t,ProblemState &ps,int arrNo,TinyMap<int8_t,Value*> &idxs,Value* value) {
    return gs.builder->CreateStore(createGEP(gs,t,ps,arrNo,idxs),value);
}

/*take an array record, a group of master indexs to index into it in order and a group
 *of literal deltas and convert into a group of LLVM Value* of current variables to
 *index into the array
 */
void getActualIdxs(Value **output,LLCompilerState& gs,ArrRec *r,int8_t *masterIdxs,int8_t *deltas) {
    int i;
    for(i=0;i<r->noDims;++r){
        output[i]=gs.curIdxVars[masterIdxs[i]];
        if(deltas[i]!=0){ //need to modify the index
            output[i]=gs.builder->CreateAdd(output[i],CST(deltas[i]));
        }
    }
}

Value* getAccumInitialiser(Operation op,EltType type) {
    switch(op){
        /*OP(ADD,0.0f,0LL);
          OP(MULTIPLY,1.0f,1LL);*/
    case ADD: return mkConst(type,0);
    case MULTIPLY: return mkConst(type,1);
    case MAX: return mkConst(type,int64_t(1)<<(8*(type & 7)-1));
    case MIN: return mkConst(type,-1*(int64_t(1)<<(8*(type & 7)-1)));
    case OR: return mkConst(type,0);
    case AND: return mkConst(type,1);
    default:panic_if(1,"unhandled reduce start value",op);
    }
#undef OP
}

/*Logical expressions:
 *OR: bool x bool -> bool
 *BOR: promote_T(bool) x T -> T
 */
constexpr int CC(int o,int t) { return (o|t); }
/*nasty macros ahead... the below can't be done with member function pointers
 *as the IRBuilder member functions aren't as regular as they look, but have
 *lots of different default arguments. So using pointers to member functions
 *just gets nasty, tedious and much more work.
 */
Value* makeLLVMBinOp(LLCompilerState &gs,Operation op,EltType type,Value **args) {
    Value *a0=args[0];
    //unary operations --------------------------------------------------
    if(opArity[op]==1){
#define OP(a,b) case a: return 0
        switch(op){ //powi
            OP(SQRT,sqrt);
            OP(ABS,0);//fabs
            OP(FLOOR,floor);
            OP(EXP,exp);
            OP(LOG,log);
            OP(SIN,sin);
            OP(COS,cos);
//fma
        }
#undef OP
        panic_if(1,"unhandled unary operator",op);
    }
    //binary operations -------------------------------------------------
    Value *a1=args[1];
    if(opArity[op]==2){
#define SO(baseOp,typeInfo,llvmMFn) case (baseOp|typeInfo): return gs.builder->llvmMFn(a0,a1)
        switch(op | (type & 0xB0)) {
            //"regular" operations
            SOD3(BOR,CreateOr,CreateOr,CreateOr);
            SOD3(BAND,CreateAnd,CreateAnd,CreateAnd);
            SOD3(BANDNOT,CreateICmpULE,CreateICmpSLE,CreateFCmpOLE);
            SOD3(BXOR,CreateXor,CreateXor,CreateXor);
            SOD3(EQ,CreateICmpEQ,CreateICmpEQ,CreateFCmpOEQ);
            SOD3(LT,CreateICmpULT,CreateICmpSLT,CreateFCmpOLT);
            SOD3(GT,CreateICmpUGT,CreateICmpSGT,CreateFCmpOGT);
            SOD3(LE,CreateICmpULE,CreateICmpSLE,CreateFCmpOLE);
            SOD3(GE,CreateICmpUGE,CreateICmpSGE,CreateFCmpOGE);
            SOD3(ADD,CreateNUWAdd,CreateNSWAdd,CreateFAdd);
            SOD3(SUBTRACT,CreateNUWSub,CreateNSWSub,CreateFSub);
            SOD3(MULTIPLY,CreateNUWMul,CreateNSWMul,CreateFMul);
            //"per-type special case" operations
            SO(DIVIDE,UINT,CreateAdd);
            SO(POWER,UINT,CreateSub);
            SO(DIVIDE,SINT,CreateAdd);
            SO(POWER,SINT,CreateSub);
            SO(DIVIDE,FLOAT,CreateFAdd);
            SO(POWER,FLOAT,CreateSub);
            SO(OR,BOOL,CreateOr);
            SO(AND,BOOL,CreateAnd);
            SO(ANDNOT,BOOL,CreateFMul);//CreateNot
            SO(XOR,BOOL,CreateXor);
            //SO(ADD,0,CreateAdd);
            //really irregular expressions
#undef SO
#define SO(nm,ty,op) case CC(nm,ty): return gs.builder->CreateSelect(gs.builder->op(a0,a1),a0,a1)
            SOD3(MIN,CreateICmpULT,CreateICmpSLT,CreateFCmpOLT);
            SOD3(MAX,CreateICmpUGT,CreateICmpSGT,CreateFCmpOGT);
#undef SO
        }
        panic_if(1,"unhandled binary operator",op);
    }
    //ternary operations ------------------------------------------------
    Value *a2=args[2];
    if(opArity[op]==3){
        switch(op){
        case SELECT: return gs.builder->CreateSelect(a0,a1,a2);
        case FMA: return gs.builder->CreateSelect(a0,a1,a2);
        }
        panic_if(1,"unhandled ternary operator",op);
    }
    panic_if(1,"unhandled operator",op);
}

Exp::Exp(uint8_t snodeType,EltType stype) {
    output=0;
    llvmTemp=0;
    type=stype;
    loopBodyIdx=0;
    nodeType=snodeType;
}

Exp::~Exp() {
    //nothing
}

//note this visits the "children" before processing the node
void Exp::recVisitorBase(VFn vfn,void* opaque) {
    recVisitor(vfn,opaque);
    (this->*vfn)(opaque);
}

void Exp::wipeLLVM(void*) {
    llvmTemp=0;
    wipeSpecific(null_ptr); //call virtual function
}

void Exp::displayBase(void *opaque) {
    std::ostream *s=((DisplayRec*)opaque)->s;
    *s<<"(";
    display(opaque); //call virtual function
    *s<<")";
}

Collection::Collection(vector<ExpPtr> &selts) : Exp(Collection::TYPECODE,0), elts(selts) {

}

Collection::~Collection() {

}

void Collection::recVisitor(VFn vfn,void* opaque) {
    for(auto e : elts) {
        e->recVisitorBase(vfn,opaque);
    }
}

Exp* Collection::childSatisfies(EPred pred,void* opaque) {

}

Value* Collection::generateSpecific(LLCompilerState& global) {

}

void Collection::wipeSpecific(void*) {
    //dnt
}

Value* Collection::outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,LoopStatus &ls,uint8_t tgtLoopBodyIdx) {
    return null_ptr;
}

void Collection::display(void* opaque) {
    std::ostream *s=((DisplayRec*)opaque)->s;
    *s<<"Collection";
}

VarRec::VarRec(Value* sllvmTemp,EltType stype) : Exp(VarRec::TYPECODE,stype) {
    llvmTemp=sllvmTemp;
}

VarRec::VarRec(ArrRec *a,EltType stype) : Exp(VarRec::TYPECODE,stype) {

}

VarRec::~VarRec() {

}

void VarRec::recVisitor(VFn vfn,void* opaque) {
    //dnt
}

//precondition: all indices to satisfy var occur in ls
Value* VarRec::outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,LoopStatus &ls,uint8_t tgtLoopBodyIdx) {
    //associate actual indices to variables
    Value* idxVs[MAX_ARR_DIMS];
    for(int i=0;i<noIdxs;++i){
        for(int j=0;j<ls.depth;++j){
            if(idxs[i]==ls.globalExtents[j]){
                idxVs[i]=ls.inductionVars[j];
                if(deltas[i]!=0){
                    //add value to induction var
                }
                break;
            }
        }
    }
    //for appropriate GEP vvvv
    return 0;//loadValueFromArraySlot();
}

Exp* VarRec::childSatisfies(EPred pred,void* opaque) {
    return 0;
}

NumLiteral::NumLiteral(int64_t v) : Exp(NumLiteral::TYPECODE,SINT | W64) {
    sMem=v;
    type=SINT | W64;
}

NumLiteral::NumLiteral(uint64_t v) : Exp(NumLiteral::TYPECODE,UINT | W64) {
    uMem=v;
    type=UINT | W64;
}

NumLiteral::NumLiteral(double v) : Exp(NumLiteral::TYPECODE,FLOAT | W64) {
    fMem=v;
    type=FLOAT | W64;
}

NumLiteral::~NumLiteral() {

}

void NumLiteral::recVisitor(VFn vfn,void* opaque) {
    //dnt
}

Value* NumLiteral::outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,LoopStatus &ls,uint8_t tgtLoopBodyIdx) {
    /*if(type & FLOAT){
        return ConstantDouble(gs.TheLLVMContext,APFloat(fMem));
        }else*/{
        return ConstantInt::get(getGlobalContext(),APInt(32,sMem));
    }
}

Exp* NumLiteral::childSatisfies(EPred pred,void* opaque) {
    return 0;
}

Value*
NumLiteral::generateSpecific(LLCompilerState& global) {

}

void
NumLiteral::wipeSpecific(void*) {
    //dnt
}

void NumLiteral::display(void* opaque) {
    std::ostream *s=((DisplayRec*)opaque)->s;
    if(type==FLOAT){
        *s<<"LitDouble "<<fMem;
    }else if(type==UINT){
        *s<<"LitUInt "<<uMem;
    }else if(type==SINT){
        *s<<"LitUInt "<<sMem;
    }else{
        *s<<"Lit malformed";
    }
}

RandArr::RandArr(EltType stype,uint8_t srandType) : Exp(RandArr::TYPECODE,stype) {
    type=stype;
    randType=srandType;
}

RandArr::~RandArr() {

}

void RandArr::recVisitor(VFn vfn,void* opaque) {
    //dnt
}

Value* RandArr::outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,LoopStatus &ls,uint8_t tgtLoopBodyIdx) {
    return null_ptr;
}

Exp* RandArr::childSatisfies(EPred pred,void* opaque) {
    return 0;
}

Value* RandArr::generateSpecific(LLCompilerState& global) {

}

void RandArr::wipeSpecific(void*) {
    //dnt
}

void RandArr::display(void* opaque) {
    std::ostream *s=((DisplayRec*)opaque)->s;
    *s<<"RandomArr";
}


UFnApp::UFnApp(ExpPtr sinput,void *sfnPtr,EltType stype) : Exp(RandArr::TYPECODE,stype) {
    input=sinput;
    fnPtr=sfnPtr;
    inT=0;
    outT=0;
}

UFnApp::~UFnApp() {

}

void UFnApp::recVisitor(VFn vfn,void* opaque) {
    //dnt
}

Value* UFnApp::outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,LoopStatus &ls,uint8_t tgtLoopBodyIdx) {
    return null_ptr;
}

Exp* UFnApp::childSatisfies(EPred pred,void* opaque) {
    return 0;
}

Combiner::Combiner(Operation soperation,EltType stype,ExpPtr i0,ExpPtr i1,ExpPtr i2) : Exp(RandArr::TYPECODE,stype) {
    inputs[0]=i0;
    inputs[1]=i1;
    inputs[2]=i2;
    operation=soperation;
}

Combiner::~Combiner() {

}

void Combiner::recVisitor(VFn vfn,void* opaque) {
    int i;
    for(i=0;i<opArity[operation];++i){
        inputs[i]->recVisitorBase(vfn,opaque);
    }
}

Value* Combiner::outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,LoopStatus &ls,uint8_t tgtLoopBodyIdx) {
    Value* vs[3];
    int i;
    for(i=0;i<opArity[operation];++i){
        vs[i]=inputs[i]->outputNodeIf(gs,ps,ls,tgtLoopBodyIdx);
        if(vs[i]==null_ptr){ //reload from array
            vs[i]=0;
        }
    }
    Value *v=llvmTemp;
    if(v==null_ptr && loopBodyIdx==tgtLoopBodyIdx){ //only create code if not already processed
        //cccc
    }
    return v;
}

Exp* Combiner::childSatisfies(EPred pred,void* opaque) {
    return 0;
}

Traversal::Traversal(Collection &sbody,Collection &saccumulations,EltType stype)
 : Exp(RandArr::TYPECODE,stype),body(sbody), accumulations(saccumulations){

}

Traversal::~Traversal() {

}

void Traversal::recVisitor(VFn vfn,void* opaque) {
    for(auto e : body.elts) {
        e->recVisitorBase(vfn,opaque);
    }
    for(auto e1 : accumulations.elts) {
        e1->recVisitorBase(vfn,opaque);
    }
    //fill-in
}

Value* Traversal::outputNodeIfSpecific(LLCompilerState &gs,ProblemState &ps,LoopStatus &ls,uint8_t tgtLoopBodyIdx) {
    return null_ptr;
}


Exp* Traversal::childSatisfies(EPred pred,void* opaque) {
    return 0;
}

Value* writeInitOfType() {
    return 0;
}

Value* typeConvert(LLCompilerState &gs,EltType outT,Value *v,EltType inT) {
    const int SAME=0;
    const int SMALLER=1;
    const int LARGER=2;
    if(inT==outT){ //type includes fundamental type, vectorness and size
        return v;
    }
#define CC(oT,iT,rs) ((rs<<4)|(fundamentalTypeCode(oT)<<2)|fundamentalTypeCode(iT))
#define OP(oT,iT,rs,fn) case CC(oT,iT,rs): return gs.builder->fn(v,llvmType(outT))
#define OP2(oT,iT,fn) OP(oT,iT,SAME,fn); OP(oT,iT,SMALLER,fn); OP(oT,iT,LARGER,fn)
    int relSz=(outT & 7)==(inT & 7)?0:((outT & 7)>(inT & 7)?1:2);
    int vecRelSz=0;//outT.bits==inT.bits?0:(outT.bits>inT.bits?1:2);
    if(vecRelSz==0){
        //case(T,T,SAME) can't occur
        if(((inT==UINT && outT==SINT)||(inT==SINT && outT==UINT)) &&
           relSz==0){ //conversion is purely conceptual
            return v;
        }
        switch(CC(inT,outT,relSz)){
#define TC(iT,oT,smallFn,bigFn) OP(iT,oT,SMALLER,smallFn);OP(iT,oT,LARGER,bigFn)
            OP2(FLOAT,UINT,CreateUIToFP);
            OP2(FLOAT,SINT,CreateSIToFP);
            OP2(UINT,FLOAT,CreateFPToUI);
            OP2(SINT,FLOAT,CreateFPToSI);
            TC(FLOAT,FLOAT,CreateFPTrunc,CreateFPExt);
            TC(UINT,UINT,CreateTrunc,CreateZExt);
            TC(SINT,SINT,CreateTrunc,CreateSExt);
            TC(SINT,UINT,CreateTrunc,CreateSExt);
            TC(UINT,SINT,CreateTrunc,CreateZExt);
#undef TC
#undef OP2
#undef OP
        }
    }
    //reaching here we didn't match anything
    panic_if(1,"unhandled type conversion",outT);
}

Value* Exp::generateCode(LLCompilerState& gs) {
    if(llvmTemp!=0){
        return llvmTemp;
    }
    Value *rv=generateSpecific(gs);
    if(output!=0){
        Value* actualIdxs[LLCompilerState::MAX_RANGES];
        getActualIdxs(actualIdxs,gs,output->r,output->idxs,output->deltas);
        storeValueToArraySlot(gs,type,output->r,actualIdxs,rv);
    }
    return rv;
}

Value* VarRec::generateSpecific(LLCompilerState& gs) {
    int type;
    return loadValueFromArraySlot(gs,type,0,0);
}

void VarRec::wipeSpecific(void*) {
    //nothing
}

void VarRec::display(void* opaque) {
    std::ostream *s=((DisplayRec*)opaque)->s;
    *s<<"VarRec "<<r->name;
    if(noIdxs>0){
        *s<<".";
        int i;
        for(i=0;i<noIdxs;++i){
            int idx=idxs[i],delta=deltas[i];
            bool brackets=(idx==0 || delta!=0);
            if(brackets){
                *s<<"(";
            }
            if(idx!=0){
                //*s<<"V"<<ps->extents[idx];
            }
            if(idx==0||delta!=0){
               if(delta>=0){
                    *s<<"+";
                }
                *s<<static_cast<int>(delta);
            }
            if(brackets){
                *s<<")";
            }
        }
    }
}

Value* UFnApp::generateSpecific(LLCompilerState& gs) {
    return 0;
}

void UFnApp::wipeSpecific(void*) {
    //dnt
}

void UFnApp::display(void* opaque) {
    std::ostream *s=((DisplayRec*)opaque)->s;
    *s<<"UFnApp";
}

Value* Combiner::generateSpecific(LLCompilerState& gs) {
    Value *as[3];
    for(int i=0;i<opArity[operation];++i){
        //UGH: first argument of select is special case
        if(inputs[i]!=0 && !(i==0 && operation==SELECT)){
            as[i]=typeConvert(gs,type,inputs[i]->generateCode(gs),inputs[i]->type);
        }
    }
    return makeLLVMBinOp(gs,operation,type,as);

}

void Combiner::wipeSpecific(void*) {
    //dnt
}

void Combiner::display(void* opaque) {
    std::ostream *s=((DisplayRec*)opaque)->s;
    *s<<"Combiner "<<opNames[operation];
}


#if 0
struct TravDB {
    Function *function;
    std::vector<Value*> accumVarsIn;
    std::vector<Value*> accumVarsOut;
};

Value* Traversal::generateInnermost(LLCompilerState& gs,int idx,TravDB &db) {
    const int gsIdx=idxs[idx];
    const int loopStart=gs.traversalRanges[gsIdx][0];
    const int loopLimit=gs.traversalRanges[gsIdx][1];
    const int loopStep=1; //do SIMD in future
    //write loop header-----------------------------------------------------------
    Function *db.function=gs.builder->GetInsertBlock()->getParent();
    BasicBlock *PreheaderBB=gs.builder->GetInsertBlock();
    BasicBlock *headerLabel=BasicBlock::Create(getGlobalContext(),"loop",db.function);
    gs.builder->CreateBr(headerLabel); //explicit jump from current block to LoopBB
    gs.builder->SetInsertPoint(headerLabel); // Start insertion in headerLabel
    //actual phi nodes need to go right after branch in, but fill them in later
    PHINode* PN=gs.builder->CreatePHI(Type::getInt32Ty(getGlobalContext()),2,"ctr");
    Value *incCtr=gs.builder->CreateAdd(PN,CST(loopStep));
    std::vector<PHINode*> phis; //phi nodes for variables in accumulations
    for(auto it : accumulations){
        phis.push_back(gs.builder->CreatePHI(llvmType(it->type),2));
    }
    //write "main loop body" by traversing input vars-----------------------------
    for(auto it : body){
        it->generateCode(gs);
    }
    BasicBlock *curBB=gs.builder->GetInsertBlock();
    PN->addIncoming(CST(loopStart),PreheaderBB);
    PN->addIncoming(incCtr,curBB);
    for(int acc=0;acc<accumulations.size();++acc){ //main body expressions
        PHINode *phi=phis[acc];
        Combiner *e2=accumulations[acc];
        e2->inputs[1]=ExpPtr(new VarRec(phi,accumulations[acc]->type)); //patch dummy containing phi-node value
        db.accumVarsIn[acc]=e2->generateCode(gs); //write instruction for accumulation
        e2->inputs[1]=0; //be safe
        phi->addIncoming(e2->llvmTemp,curBB);
        //TODO fix
#if 0
        auto it2=insDB.find(make_pair(e2->operation,0));
        panic_if(it2==insDB.end(),"unhandled unary operator");
        phi->addIncoming(it2->second.neutral,PreheaderBB); //initial value
#endif
    }
    //write loop prologue---------------------------------------------------------
    BasicBlock *AfterBB=BasicBlock::Create(getGlobalContext(),"afterloop",db.function);
    // Insert the conditional branch into the end of LoopEndBB.
    Value* loopCond=gs.builder->CreateICmpSLT(incCtr,CST(loopLimit),"cond");
    gs.builder->CreateCondBr(loopCond,headerLabel,AfterBB);
    // Any new code will be inserted in AfterBB.
    gs.builder->SetInsertPoint(AfterBB);
}
#endif
Value* Traversal::generateSpecific(LLCompilerState& gs) {
#if 0
    assert(noIdxs==1); //temporary
    int idx;
    for(idx=0;idx<noIdxs;++idx){
        const int gsIdx=idxs[idx];
        const int loopStart=gs.traversalRanges[gsIdx][0];
        const int loopLimit=gs.traversalRanges[gsIdx][1];
        const int loopStep=1; //do SIMD in future
        bool isUnitStride=false;
        if(addTiming){
            Value *start=0;
        }
        //write loop header-----------------------------------------------------------
        Function *TheFunction=gs.builder->GetInsertBlock()->getParent();
        BasicBlock *PreheaderBB=gs.builder->GetInsertBlock();
        BasicBlock *headerLabel=BasicBlock::Create(getGlobalContext(),"loop",TheFunction);
        gs.builder->CreateBr(headerLabel); //explicit jump from current block to LoopBB
        gs.builder->SetInsertPoint(headerLabel); // Start insertion in headerLabel
        //actual phi nodes need to go right after branch in, but fill them in later
        PHINode* PN=gs.builder->CreatePHI(Type::getInt32Ty(getGlobalContext()),2,"ctr");
        Value *incCtr=gs.builder->CreateAdd(PN,CST(loopStep));
        std::vector<PHINode*> phis; //phi nodes for variables in accumulations
        for(auto it : accumulations){
            phis.push_back(gs.builder->CreatePHI(llvmType(it->type),2));
        }
        //write "main loop body" by traversing input vars-----------------------------
        for(auto it : body){
            it->generateCode(gs);
        }
        BasicBlock *curBB=gs.builder->GetInsertBlock();
        PN->addIncoming(CST(loopStart),PreheaderBB);
        PN->addIncoming(incCtr,curBB);
        for(int acc=0;acc<accumulations.size();++acc){ //main body expressions
            PHINode *phi=phis[acc];
            Combiner *e2=accumulations[acc];
            e2->inputs[1]=ExpPtr(new VarRec(phi,accumulations[acc]->type)); //patch dummy containing phi-node value
            e2->generateCode(gs); //write instruction for accumulation
            e2->inputs[1]=0; //be safe
            phi->addIncoming(e2->llvmTemp,curBB);
            //TODO fix
#if 0
            auto it2=insDB.find(make_pair(e2->operation,0));
            panic_if(it2==insDB.end(),"unhandled unary operator");
            phi->addIncoming(it2->second.neutral,PreheaderBB); //initial value
#endif
        }
        //write loop prologue---------------------------------------------------------
        BasicBlock *AfterBB=BasicBlock::Create(getGlobalContext(),"afterloop",TheFunction);
        // Insert the conditional branch into the end of LoopEndBB.
        Value* loopCond=gs.builder->CreateICmpSLT(incCtr,CST(loopLimit),"cond");
        gs.builder->CreateCondBr(loopCond,headerLabel,AfterBB);
        // Any new code will be inserted in AfterBB.
        gs.builder->SetInsertPoint(AfterBB);
        if(addTiming){
            Value *end=0;
            Value *taken=gs.builder->CreateAdd(end,start);
            //TODO: reporting
        }
    }
#endif
}

void Traversal::wipeSpecific(void*) {
    //dnt
/*    int i;
    for(auto it : body.elts){
        it->wipeLLVM(null_ptr);
    }
    for(auto it2 : accumulations.elts){
        it2->wipeLLVM(null_ptr);
        }*/
}

void Traversal::display(void* opaque) {
    std::ostream *s=((DisplayRec*)opaque)->s;
    *s<<"Combiner";
}

//since this is a DAG, providing we always instantiate the children before the parents the dependencies are ok
//loop body indexes are monotonically increasing from parents to children
/*Value* generateLoopBody(LLCompilerState &gs,ProblemState &ps,LoopStatus &ls,uint8_t tgtLoopBodyIdx,ExpPtr e) {
    if(e->tgtLoopBodyIdx<tgtLoopBodyIdx){
        return null_ptr;
    }else{
        if(e->output==null_ptr){

        }
        if(e->tgtLoopBodyIdx==tgtLoopBodyIdx){ //if we're at the level we need to emit code for ourselves
        }//otherwise nothing
    }
    }*/

Value* Exp::outputNodeIf(LLCompilerState &gs,ProblemState &ps,LoopStatus &ls,uint8_t tgtLoopBodyIdx) {
    if(loopBodyIdx<tgtLoopBodyIdx){
        return null_ptr;
    }
    Value *v=outputNodeIfSpecific(gs,ps,ls,tgtLoopBodyIdx);
    return v;
/*    if(e->output==null_ptr){

    }
    if(e->loopBodyIdx==loopBodyIdx){ //if we're at the level we need to emit code for ourselves
    }//otherwise nothing*/
}

Function* createFunctionFromDAG(LLCompilerState &gs,ExpPtr e,const char* const fnName) {
    //generate function prologue ---------------------------------------------------
    // Make the function type:  double(double,double) etc.
    Function *TheFunction;
#if 0
    int noArgs=NO_ARGS;
    std::vector<Type*> incomingArgs;
    int i;
    for(i=0;i<noArgs;++i){
        incomingArgs.push_back(llvm1DRealArrayTypeArg(32));
    }
//    FunctionType *FT = FunctionType::get(Type::getInt32Ty(getGlobalContext()),incomingArgs, false);
    FunctionType *FT = FunctionType::get(llvmRealTypeBase,incomingArgs, false);
    std::string name(fnName);
    Function *TheFunction = Function::Create(FT, Function::ExternalLinkage, name, TheModule);
    // Set names for all arguments.
    std::vector<std::string> Args({"output","input","params","distribs"});
    Function::arg_iterator AI;
    unsigned Idx ;
    for (Idx=0, AI = TheFunction->arg_begin(); Idx < Args.size(); ++AI, ++Idx){
        AI->setName(Args[Idx]);
    }
    // Create a new basic block to start insertion into.
    BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", TheFunction);
    Builder->SetInsertPoint(BB);
    //create standard loaded pointers to the argument arrays
    for (Idx=0, AI = TheFunction->arg_begin(); Idx < Args.size(); ++AI, ++Idx){
        argumentOrig[Idx]=(Value*)AI;
        argumentPtrs[Idx]=Builder->CreateLoad((Value*)AI,false);
    }
    //take vectors of inputs to SSA-register values
    int type;
    for(type=INPUT;type<=PARAMETER;++type){
        for(int i=0;i<gs.noVars[type];++i){
            copyValFromArrayIns(argumentPtrs[type],gs.specialVars[type][i],i,true);
        }
    }
#endif
    //generate the actual work instructions ----------------------------------------
    e->generateCode(gs);
    //generate the function prologue -----------------------------------------------
    gs.builder->CreateRet(CST(0)); // Finish off the function
    if(gs.verbose){
        gs.TheModule->dump();
    }
    verifyFunction(*TheFunction); // Validate generated code, checking for consistency
    if(gs.enableLowlevelOpt){
        gs.FPM->run(*TheFunction); // Optimize the function
        if(gs.verbose){
            gs.TheModule->dump();
        }
    }
    return TheFunction;
}

/*Typing:
 *A_idxsetA = B_idxsetB op C_idxsetC
 *where idxsetA only contains indexes from B and C,
 *then indexset of A can be constructed
 *Likewise for reductions
 *
 *
 */


/*Exp* is a DAG, so need to assign llvmTemp in process.
 *Also MUST return if llvmTemp set since other node content may not be valid.
 */
void test() {

}
#define S(X) printf("Sizeof %s=%d\n",#X,sizeof(X));
int main(int argc,char* argv[]) {
    S(Exp);
    S(ExpPtr);
    S(Exp*);
    S(VarRec);
    S(UFnApp);
    S(Combiner);
    S(Traversal);
#if 1
//very simple test code
    ExpPtr AAP(new NumLiteral(double(2))/*0*/),BAP(new NumLiteral(double(2))/*0*/),CAP(new NumLiteral(double(2))/*0*/);
    ExpPtr UAP(new NumLiteral(double(2)));
    ExpPtr VAP(new Combiner(MULTIPLY,FLOAT|W64,AAP,AAP));
    ExpPtr WAP(new Combiner(ADD,FLOAT|W64,AAP,BAP));
    ExpPtr XAP(new Combiner(MULTIPLY,FLOAT|W64,BAP,CAP));
    ExpPtr YAP(new Combiner(SUBTRACT,FLOAT|W64,AAP,WAP));
    ExpPtr ZAP(new Combiner(DIVIDE,FLOAT|W64,XAP,YAP));
    ExpPtr SAP(new Combiner(MULTIPLY,FLOAT|W64,VAP,VAP));
    ExpPtr TAP(new Combiner(ADD,FLOAT|W64,SAP,UAP));
    DisplayRec dr;
    dr.ps=0;
    dr.s=&std::cout;
    ZAP->recVisitorBase(&Exp::displayBase,&dr);
#endif
    return 0;
}

