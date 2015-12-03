#!/usr/bin/env bash

if [[ -n "$LLVM_DIR" ]]; then
    BUILD_DIR="$LLVM_DIR/build"
fi

if [[ -z "$BUILD_DIR" ]]; then
    echo "Error! BUILD_DIR not set! Don't know how to find swift binary."
    exit 1
fi

SWIFT="$BUILD_DIR/bin/swift"
CLANG="$BUILD_DIR/bin/clang"
CLANGPP="$BUILD_DIR/bin/clang++"
SDKROOT=$(xcrun --show-sdk-path -sdk macosx) || exit 1

benchmark() {
    local NAME=$1

    echo "Compiling swift/cpp for benchmark $NAME"

    set -e
    set -x

    # Remove old object files/binaries.
    rm -rfv $NAME.*.o $NAME.*_bin

    # Compile swift benchmark.
    $SWIFT -O $NAME.swift -o $NAME.swift.o -c
    $CLANG $NAME.swift.o -o $NAME.swift_bin -Wl,-rpath -Wl,"$BUILD_DIR/lib/swift/macosx" -L "$BUILD_DIR/lib/swift/macosx"

    # Compile CXX benchmark.
    $CLANGPP -O3 -c $NAME.cpp -o $NAME.cpp.o -isysroot "$SDKROOT" $CFLAGS $CXXFLAGS -DLOG=0
    $CLANGPP -O3  $NAME.cpp.o -o $NAME.cpp_bin -isysroot "$SDKROOT" $CFLAGS $CXXFLAGS

    # Run swift benchmark.
    ./$NAME.swift_bin

    # Compile CXX benchmark
    ./$NAME.cpp_bin

    set +x
    set +e
}

(cd RC4 && benchmark RC4)
(cd ObjInst && benchmark ObjInst)
(cd Ackermann && benchmark Ackermann)
