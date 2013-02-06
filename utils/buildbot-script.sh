#!/bin/sh -ex

# The Jenkins workspace is set up as follows:
# - LLVM checked out to ${WORKSPACE}/llvm
# - Clang checked out to ${WORKSPACE}/clang
# - Swift checked out to ${WORKSPACE}/swift
# Jenkins is set up to restore the repositories to pristine state before
# building, so we rebuild from scratch every time.

# Flags for testing:
# SKIP_BUILD_LLVM -- set to skip building LLVM/Clang
# SKIP_BUILD_SWIFT -- set to skip building Swift
# SKIP_TEST_SWIFT -- set to skip testing Swift

# Set these to the paths of the OS X SDK and toolchain
SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk
TOOLCHAIN=/Applications/Xcode.app/Contents/Developer/Toolchains/OSX10.9.xctoolchain

# Set this to the path to the 'cmake' executable.
CMAKE=/usr/local/bin/cmake

# Make sure the variables and directories we expect to exist actually do.
test "$WORKSPACE"
test -d "$WORKSPACE"
test -d "$WORKSPACE/llvm"
test -d "$WORKSPACE/llvm/tools"
test -d "$WORKSPACE/clang"
test -d "$WORKSPACE/swift"

# Symlink clang into the llvm tree.
ln -sf "$WORKSPACE/clang" "$WORKSPACE/llvm/tools/clang"

# Build LLVM and Clang (x86 target only).
if [ \! "$SKIP_BUILD_LLVM" ]; then
  echo "--- Building LLVM and Clang ---"
  mkdir -p "$WORKSPACE/llvm/build"
  (cd "$WORKSPACE/llvm/build" &&
    "$CMAKE" -G "Unix Makefiles" \
      -DCMAKE_C_COMPILER="$TOOLCHAIN/usr/bin/clang" \
      -DCMAKE_CXX_COMPILER="$TOOLCHAIN/usr/bin/clang++" \
      -DCMAKE_CXX_FLAGS="-stdlib=libc++" \
      -DCMAKE_EXE_LINKER_FLAGS="-stdlib=libc++" \
      -DCMAKE_SHARED_LINKER_FLAGS="-stdlib=libc++" \
      -DLLVM_TARGETS_TO_BUILD="X86" \
      .. &&
    make -j) || exit 1
fi

# Build Swift.
if [ \! "$SKIP_BUILD_SWIFT" ]; then
  echo "--- Building Swift ---"
  mkdir -p "$WORKSPACE/swift/build"
  (cd "$WORKSPACE/swift/build" &&
    "$CMAKE" -G "Unix Makefiles" \
      -DCMAKE_C_COMPILER="$TOOLCHAIN/usr/bin/clang" \
      -DCMAKE_CXX_COMPILER="$TOOLCHAIN/usr/bin/clang++" \
      -DSWIFT_PATH_TO_CLANG_SOURCE="$WORKSPACE/llvm/tools/clang" \
      -DSWIFT_PATH_TO_CLANG_BUILD="$WORKSPACE/llvm/build" \
      -DSWIFT_PATH_TO_LLVM_SOURCE="$WORKSPACE/llvm" \
      -DSWIFT_PATH_TO_LLVM_BUILD="$WORKSPACE/llvm/build" \
      .. &&
    make -j) || exit 1
fi

# Run the Swift tests.
if [ \! "$SKIP_TEST_SWIFT" ]; then
  export SWIFT="$WORKSPACE/swift/build/bin/swift"
  echo "--- Running Swift Tests ---"
  (cd "$WORKSPACE/swift/build" &&
    "$WORKSPACE/llvm/build/bin/llvm-lit" -sv test) || exit 1
fi
