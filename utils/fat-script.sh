#!/bin/sh -ex

# - LLVM checked out to ${WORKSPACE}/llvm
# - Clang checked out to ${WORKSPACE}/clang
# - Swift checked out to ${WORKSPACE}/swift

# Flags for testing:
# SKIP_BUILD_LLVM -- set to skip building LLVM/Clang
# SKIP_BUILD_SWIFT_TOOLS -- set to skip building Swift tools
# SKIP_BUILD_SWIFT_STDLIB -- set to skip building Swift stdlib
# SKIP_TEST_SWIFT -- set to skip testing Swift
# SKIP_TEST_SWIFT_PERFORMANCE -- set to skip testing Swift performance

# The -release flag enables a release build.
if [ "$1" = "-release" ]; then
  BUILD_TYPE=RelWithDebInfo
else
  BUILD_TYPE=Debug
fi

# Set this to the path to the 'cmake' executable.
CMAKE=/usr/local/bin/cmake

# Set this to the install prefix for release builds.
INSTALL_PREFIX=/usr

# Make sure the variables and directories we expect to exist actually do.
test "$WORKSPACE"
test -d "$WORKSPACE"
test -d "$WORKSPACE/llvm"
test -d "$WORKSPACE/llvm/tools"
test -d "$WORKSPACE/clang"
test -d "$WORKSPACE/swift"

# Make sure install-test-script.sh is available alongside us.
INSTALL_TEST_SCRIPT="$(dirname "$0")/install-test-script.sh"

if [ \! -x "$INSTALL_TEST_SCRIPT" ]; then
  echo "Install test script $INSTALL_TEST_SCRIPT is unavailable or not executable!"
  exit 1
fi

# Symlink clang into the llvm tree.
ln -sf "$WORKSPACE/clang" "$WORKSPACE/llvm/tools/clang"

# Create a fresh directory for the Swift Clang module cache.
if [ -e "$WORKSPACE/swift-module-cache" ]; then
  rm -rf "$WORKSPACE/swift-module-cache" || exit 1
fi
mkdir -p "$WORKSPACE/swift-module-cache"

# Make extra sure it's empty.
if [ "$(ls -A "$WORKSPACE/swift-module-cache")" ]; then
  echo "Module cache not empty! Aborting."
  exit 1
fi

CC=`xcrun -find clang`
CXX=`xcrun -find clang++`

# Build LLVM and Clang (x86 and arm targets only).
if [ \! "$SKIP_BUILD_LLVM" ]; then
  echo "--- Building LLVM and Clang ---"
  mkdir -p "$WORKSPACE/llvm/build"
  (cd "$WORKSPACE/llvm/build" &&
    "$CMAKE" -G "Unix Makefiles" \
      -DCMAKE_C_COMPILER="$CC" \
      -DCMAKE_CXX_COMPILER="$CXX" \
      -DCMAKE_CXX_FLAGS="-stdlib=libc++" \
      -DCMAKE_EXE_LINKER_FLAGS="-stdlib=libc++" \
      -DCMAKE_SHARED_LINKER_FLAGS="-stdlib=libc++" \
      -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
      -DLLVM_TARGETS_TO_BUILD="X86;ARM;ARM64" \
      -DLLVM_ENABLE_ASSERTIONS="ON" \
      .. &&
    make -j8) || exit 1
fi

# Build Swift tools.
if [ \! "$SKIP_BUILD_SWIFT_TOOLS" ]; then
  echo "--- Building Swift tools ---"
  mkdir -p "$WORKSPACE/swift/build"
  (cd "$WORKSPACE/swift/build" &&
    "$CMAKE" -G "Unix Makefiles" \
      -DCMAKE_C_COMPILER="$CC" \
      -DCMAKE_CXX_COMPILER="$CXX" \
      -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
      -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
      -DSWIFT_PATH_TO_CLANG_SOURCE="$WORKSPACE/llvm/tools/clang" \
      -DSWIFT_PATH_TO_CLANG_BUILD="$WORKSPACE/llvm/build" \
      -DSWIFT_PATH_TO_LLVM_SOURCE="$WORKSPACE/llvm" \
      -DSWIFT_PATH_TO_LLVM_BUILD="$WORKSPACE/llvm/build" \
      -DSWIFT_BUILD_STDLIB="NO" \
      -DLLVM_ENABLE_ASSERTIONS="ON" \
      .. &&
    make -j8) || exit 1
fi

function build_stdlib()
{
  ARCH=$1
  SDK=$2
  VERS=$3

  SDKROOT=`xcrun -sdk $SDK -show-sdk-path`
  CC=`xcrun -sdk $SDK -find clang`
  CXX=`xcrun -sdk $SDK -find clang++`

  # Strip extensions like version or .internal from SDK
  OS=`echo $SDK | sed -E -e 's/^([a-zA-Z]+).*/\1/'`

  # FIXME CMAKE_OSX_DEPLOYMENT_TARGET falls over on iOS version numbers

  SWIFT_ASSERTS=ON
  if [ "$BUILD_TYPE" = "Debug" ] ; then
    SWIFT_OPTIMIZED=OFF
  else
    SWIFT_OPTIMIZED=ON
  fi

  echo "--- Building Swift stdlib $OS $VERS $ARCH ---"
  mkdir -p "$WORKSPACE/swift/build/stdlib/$OS-$ARCH"
  (cd "$WORKSPACE/swift/build/stdlib/$OS-$ARCH" &&
    "$CMAKE" -G "Unix Makefiles" \
      -DCMAKE_TOOLCHAIN_FILE=cmake/$OS.cmake \
      -DCMAKE_SYSTEM_PROCESSOR="$ARCH" \
      -DCMAKE_OSX_ARCHITECTURES="$ARCH" \
      -DCMAKE_OSX_SYSROOT="$SDKROOT" \
      -DMODULES_SDK="$SDKROOT" \
      -DSWIFT_DEPLOYMENT_OS="$SDK" \
      -DSWIFT_DEPLOYMENT_TARGET="$VERS" \
      -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
      -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
      -DSWIFT_PATH_TO_CLANG_SOURCE="$WORKSPACE/llvm/tools/clang" \
      -DSWIFT_PATH_TO_CLANG_BUILD="$WORKSPACE/llvm/build" \
      -DSWIFT_PATH_TO_LLVM_SOURCE="$WORKSPACE/llvm" \
      -DSWIFT_PATH_TO_LLVM_BUILD="$WORKSPACE/llvm/build" \
      -DSWIFT_MODULE_CACHE_PATH="$WORKSPACE/swift/build/stdlib/$OS-$ARCH/swift-module-cache" \
      -DSWIFT_BUILD_TOOLS="NO" \
      -DSWIFT_COMPILER="$WORKSPACE/swift/build/bin/swift" \
      -DSWIFT_INCLUDE_DOCS="OFF" \
      -DSWIFT_OPTIMIZED="$SWIFT_OPTIMIZED" \
      -DSWIFT_ASSERTS="$SWIFT_ASSERTS" \
      -DLLVM_ENABLE_ASSERTIONS="ON" \
      ../../.. &&
    make -j8 ) || exit 1
}


# Build Swift stdlib for multiple platforms
# <arch> <SDK name in xcrun format> <deployment target version>
if [ \! "$SKIP_BUILD_SWIFT_STDLIB" ]; then
  build_stdlib x86_64 macosx 10.8
  build_stdlib x86_64 iphonesimulator 6.0
  build_stdlib arm64  iphoneos.internal 6.0

  # Symlink OS X stdlib into built tools so that the tools can be run in-place
  ln -fhs "$WORKSPACE/swift/build/stdlib/macosx-x86_64/lib/swift/"* "$WORKSPACE/swift/build/lib/swift/"
fi

# Run the Swift tests.
if [ \! "$SKIP_TEST_SWIFT" ]; then
  export SWIFT="$WORKSPACE/swift/build/bin/swift"
  export SIL_OPT="$WORKSPACE/swift/build/bin/sil-opt"
  export SWIFT_IDE_TEST="$WORKSPACE/swift/build/bin/swift-ide-test"
  export SWIFT_DEMANGLE="$WORKSPACE/swift/build/bin/swift-demangle"
  export LLDB_MODULEIMPORT_TEST="$WORKSPACE/swift/build/bin/lldb-moduleimport-test"
  echo "--- Running Swift Tests ---"
  (cd "$WORKSPACE/swift/build" &&
    "$WORKSPACE/llvm/build/bin/llvm-lit" -sv test) || exit 1
fi

# Run the Swift performance tests.
if [ \! "$SKIP_TEST_SWIFT_PERFORMANCE" ]; then
  # Currently we use the toolchain-specific Clang as our CC under test, because
  # our locally built one might not end up invoking an LD that supports
  # autolinking on older machines. We can reconsider this when it becomes useful
  # to have the C/C++ tests be running using the same LLVM basis as the Swift we
  # are testing.
  echo "--- Running Swift Performance Tests ---"
  export CLANG=`xcrun -find clang`
  export SWIFT="$WORKSPACE/swift/build/bin/swift"
  if (cd "$WORKSPACE/swift/build" &&
          "$WORKSPACE/llvm/build/bin/llvm-lit" -v benchmark \
              -j1 --output benchmark/results.json); then
      PERFORMANCE_TESTS_PASSED=1
  else
      PERFORMANCE_TESTS_PASSED=0
  fi
  echo "--- Submitting Swift Performance Tests ---"
  swift_source_revision="$("$WORKSPACE/llvm/utils/GetSourceVersion" "$WORKSPACE/swift")"
  (cd "$WORKSPACE/swift/build" &&
    "$WORKSPACE/swift/utils/submit-benchmark-results" benchmark/results.json \
        --output benchmark/lnt_results.json \
        --machine-name "matte.apple.com--${BUILD_TYPE}--x86_64--O3" \
        --run-order "$swift_source_revision" \
        --submit http://localhost:32169/submitRun) || exit 1

  # If the performance tests failed, fail the build.
  if [ "$PERFORMANCE_TESTS_PASSED" -ne 1 ]; then
      echo "*** ERROR: Swift Performance Tests failed ***"
      exit 1
  fi
fi
