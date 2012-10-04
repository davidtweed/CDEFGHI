#ifndef __LOOP_NEST_H__
#define __LOOP_NEST_H__

typedef int8_t S8;

class NestTreeSpec {
public:

};

class NestTreeInstance {
public:
    static const int MAX_DEPTH=8;
    static const int MAX_STMT=16;
    NestTreeSpec *spec;
    int64_t score;
    S8 globalVarOrder[MAX_DEPTH];
    S8 treeSpec[MAX_STMT];
    S8 nodeIds[MAX_STMT];
};

class ProbSpec {

};


#endif /*__LOOP_NEST_H__*/
