
SDK = macosx
CXX = $(shell xcrun -find clang -sdk $(SDK))
SDKPATH = $(shell xcrun --show-sdk-path -sdk $(SDK))
CXXFLAGS = -O0 -g -std=c++11 -stdlib=libc++
SWIFT = ~/work/solon/llvm/build/bin/swift
OPTS=-O0 -g

%-swift: %.swift
	$(SWIFT) $(OUTPUT_OPTION) -sdk $(SDKPATH) $< $(OPTS)

%.sil: %.swift
	$(SWIFT) $(OUTPUT_OPTION) -sdk $(SDKPATH) $< -emit-sil $(OPTS)

%: %.cpp
	$(CXX) $(OUTPUT_OPTION) -isysroot $(SDKPATH) $(LDFLAGS) $^

all: prims.o
