all: mytoyc

OBJS = parser.o  \
       codegen.o \
       main.o    \
       tokens.o  \
       native.o

LLVMCONFIG = llvm-config-6.0
CPPFLAGS = -g `$(LLVMCONFIG) --cppflags` -std=c++11
LDFLAGS = `$(LLVMCONFIG) --ldflags` -rdynamic
LIBS = `$(LLVMCONFIG) --libs`

clean:
	$(RM) -rf parser.cpp parser.hpp parser tokens.cpp tokens.hpp tokens mytoyc $(OBJS)

parser.cpp: parser.y
	bison -d -o $@ $^

parser.hpp: parser.cpp

tokens.cpp: tokens.l
	flex --header-file=$(subst .cpp,.hpp,$@) -o $@ $^

tokens.hpp: tokens.cpp

%.o: %.cpp
	$(CXX) -c $(CPPFLAGS) -o $@ $<


mytoyc: $(OBJS)
	$(CXX) -o $@ $(OBJS) $(LIBS) $(LDFLAGS)

test: mytoyc test.txt
	cat test.txt | ./$^

distclean: clean
	$(RM) -rf .depend ./parser.cpp ./parser.hpp ./tokens.cpp ./tokens.hpp

.PHONY: clean distclean

depend: .depend

.depend: parser.cpp tokens.cpp parser.hpp tokens.hpp
	$(CXX) `$(LLVMCONFIG) --cppflags` $(FLAGS) $(INCLUDE) -M *.cpp > .depend || true
	$(CXX) `$(LLVMCONFIG) --cppflags` $(FLAGS) $(INCLUDE) -M *.hpp >> .depend || true

-include .depend

