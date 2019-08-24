srcroot = /Volumes/Data/swift
objroot = /Volumes/Data/swift-DA

CXXFLAGS = -std=c++11 \
	   -stdlib=libc++ \
	   -g \
	   -I$(srcroot)/tools/swift/include \
	   -I$(srcroot)/tools/clang/include \
	   -I$(srcroot)/include \
	   -I$(objroot)/include \
	   -I$(objroot)/tools/clang/include \
	   -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS

LDFLAGS = -L$(objroot)/lib -lswiftBasic -lLLVMSupport -lcurses

main: main.cpp $(srcroot)/tools/swift/include/swift/Basic/PrefixMap.h
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o main main.cpp

clean:
	rm main
