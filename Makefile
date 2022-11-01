CXX=clang++ -std=c++17
CFLAGS= -g -O3 `llvm-config --cppflags --cxxflags --ldflags --system-libs --libs all` -std=c++17 \
-Wno-unused-function -Wno-unknown-warning-option  -fno-exceptions -fno-rtti -Wno-unused-command-line-argument

# mccomp: mccomp.cpp \n $(CXX) mccomp.cpp $(CFLAGS) -std=c++17 -o mccomp

mccomp:  astnodes.o helpers.o mccomp.o  
	$(CXX) object/*.o $(CFLAGS) -o mccomp

mccomp.o: mccomp.cpp
	$(CXX) -c mccomp.cpp $(CFLAGS) -o object/mccomp.o

astnodes.o: src/astnodes.cpp
	$(CXX) -c src/astnodes.cpp -o object/astnodes.o

helpers.o: src/helpers.cpp
	$(CXX) -c src/helpers.cpp -o object/helpers.o

# parsing.o: src/parsing.cpp
# 	$(CXX) -c src/parsing.cpp -o object/parsing.o

clean:
	rm -rf object/* mccomp 