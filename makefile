GPP=/usr/local/bin/g++
CXXOPTS=-g --std=c++11 -msse -msse2 -mssse3
DEFS=-D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS
HEADERS=arrayElisionBuilder.h linearRangeProperties.h smallBitvectors.h
OBJS=arrayElisionBuilder.o linearRangeProperties.o
LIBS=

runit: ${OBJS} ${HEADERS}
#	@echo LD runit
	${GPP} ${CXXOPTS} -o runit ${OBJS} `llvm-config --cppflags --ldflags --libs core mcjit native` ${LIBS}

tests: ${HEADERS} linearRangeTests.o linearRangeProperties.o
#	@echo LD runit
	${GPP} ${CXXOPTS} -o tests linearRangeTests.o linearRangeProperties.o `llvm-config --cppflags --ldflags --libs core mcjit native` ${LIBS}


%.o:%.cpp ${HEADERS}
#	@echo CC $@
	${GPP} -c $< -o $@ ${CXXOPTS} ${DEFS}



clean:
	rm -f *.o
