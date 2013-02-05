#ifndef __TESTCASE_GENERATOR_H__
#define __TESTCASE_GENERATOR_H__

//generate random testcases that should exercise all the code-paths

#include "arrayElisionBuilder.h"

void createTestcases(vector<ExpPtr> &tests,int noTests, int seed);

#endif /*__TESTCASE_GENERATOR_H__*/
