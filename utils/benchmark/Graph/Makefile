
SDK = macosx
CXX = $(shell xcrun -find clang -sdk $(SDK))
SDKPATH = $(shell xcrun --show-sdk-path -sdk $(SDK))
CXXFLAGS = -O0 -g -std=c++11 -stdlib=libc++
SWIFT = swift

%-swift.o: %.swift
	$(SWIFT) -c $(OUTPUT_OPTIONS) -sdk $(SDKPATH) $<

%: %.cpp
	$(CXX) $(OUTPUT_OPTIONS) -isysroot $(SDKPATH) $(LDFLAGS) $^

all: prims.o
