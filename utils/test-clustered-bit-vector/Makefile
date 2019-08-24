SRCROOT=/Volumes/Data/swift
OBJROOT=/Volumes/Data/swift-DA

HEADERS=${SRCROOT}/tools/swift/include/swift/Basic/ClusteredBitVector.h
SOURCES=${SRCROOT}/tools/swift/lib/Basic/ClusteredBitVector.cpp

generator: generator.cpp ${HEADERS} ${SOURCES}
	xcrun clang++ -std=c++11 -stdlib=libc++ -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -I${OBJROOT}/include -I${SRCROOT}/include -I${SRCROOT}/tools/swift/include -L${OBJROOT}/lib -lLLVMSupport -lcurses generator.cpp ${SOURCES} -o generator

test: test.cpp ${HEADERS} ${SOURCES}
	xcrun clang++ -g -std=c++11 -stdlib=libc++ -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -I${OBJROOT}/include -I${SRCROOT}/include -I${SRCROOT}/tools/swift/include -L${OBJROOT}/lib -lLLVMSupport -lcurses test.cpp ${SOURCES} -o test
