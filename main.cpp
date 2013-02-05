#include "arrayElisionBuilder.h"
#include <iostream>

void evaluateExpression() {

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
#if 0
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
