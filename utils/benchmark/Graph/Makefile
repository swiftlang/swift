
SDK = macosx
CXX = $(shell xcrun -find clang++ -sdk $(SDK))
SDKPATH = $(shell xcrun --show-sdk-path -sdk $(SDK))
OPT_FLAGS = -O0 -g
CXXFLAGS = $(OPT_FLAGS) -std=c++11
SWIFT_FLAGS = $(OPT_FLAGS)
SWIFT = swift

%-cpp.o: %.cpp
	$(CXX) $(OUTPUT_OPTIONS) -isysroot $(SDKPATH) $(LDFLAGS) $(CXXFLAGS) $^ -c

%: %.o
	$(CXX) -o $@ -isysroot $(SDKPATH) $(LDFLAGS) $(CXXFLAGS) $^

%-swift.o : %.swift
	$(SWIFT) $(OUTPUT_OPTIONS)  -sdk $(SDKPATH) -o $@ $(SWIFT_FLAGS) $^

prims-cpp: prims-cpp.o test-prims.o
prims-swift: prims-swift.o

all: prims-cpp

clean:
	rm -vf *.o
	rm -vf a.out
	rm -vf prims

.DEFAULT_GOAL = prims
.PHONY = all clean
