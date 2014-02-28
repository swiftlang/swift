
SDK = macosx
CXX = $(shell xcrun -find clang++ -sdk $(SDK))
SDKPATH = $(shell xcrun --show-sdk-path -sdk $(SDK))
CXXFLAGS = -O0 -g -std=c++11

%.o: %.cpp
	$(CXX) $(OUTPUT_OPTIONS) -isysroot $(SDKPATH) $(LDFLAGS) $(CXXFLAGS) $^ -c

%: %.o
	$(CXX) -o $@ -isysroot $(SDKPATH) $(LDFLAGS) $(CXXFLAGS) $^

prims: prims.o test-prims.o

all: prims

clean:
	rm -vf *.o
	rm -vf a.out
	rm -vf prims

.DEFAULT_GOAL = prims
.PHONY = all clean
