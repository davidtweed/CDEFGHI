GPP=/usr/local/bin/g++
CXXOPTS=-g --std=c++11
DEFS=-D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS
HEADERS=arrayElisionBuilder.h
OBJS=arrayElisionBuilder.o
LIBS=

runit: ${OBJS} ${HEADERS}
#	@echo LD runit
	${GPP} ${CXXOPTS} -o runit ${OBJS} `llvm-config --cppflags --ldflags --libs core mcjit native` ${LIBS}


%.o:%.cpp ${HEADERS}
#	@echo CC $@
	${GPP} -c $< -o $@ ${CXXOPTS} ${DEFS}



clean:
	rm -f *.o
