#!/bin/bash -ex -o pipefail

# The Jenkins workspace is set up as follows:
# - LLVM checked out to ${WORKSPACE}/llvm
# - Clang checked out to ${WORKSPACE}/clang
# - Swift checked out to ${WORKSPACE}/swift
# - SourceKit checked out to ${WORKSPACE}/SourceKit
# Jenkins is set up to restore the repositories to pristine state before
# building, so we rebuild from scratch every time.

# Flags for testing:
# SKIP_BUILD_LLVM -- set to skip building LLVM/Clang
# SKIP_BUILD_SWIFT -- set to skip building Swift
# SKIP_BUILD_SOURCEKIT -- set to skip building SourceKit
# SKIP_TEST_SWIFT -- set to skip testing Swift
# SKIP_TEST_SWIFT_PERFORMANCE -- set to skip testing Swift performance
# SKIP_PACKAGE_SWIFT -- set to skip packaging Swift
# SKIP_TEST_SOURCEKIT -- set to skip testing SourceKit
# SKIP_PACKAGE_SOURCEKIT -- set to skip packaging SourceKit

# The -release flag enables a release build, which will additionally build
# a package if the build and test succeeds.
if [ "$1" = "-release" ]; then
  BUILD_TYPE=RelWithDebInfo
  PACKAGE=1
  # Include a custom name to avoid picking up stale module files.
  CUSTOM_VERSION_NAME="release $(date -j '+%Y-%m-%d %H-%M-%S')"
else
  BUILD_TYPE=Debug
  PACKAGE=
  CUSTOM_VERSION_NAME=
fi

# Set these to the paths of the OS X SDK and toolchain.
SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk
TOOLCHAIN=/Applications/Xcode.app/Contents/Developer/Toolchains/OSX10.9.xctoolchain

# Set this to the path to the 'cmake' executable.
CMAKE=/usr/local/bin/cmake

# Set this to the install prefix for release builds.
INSTALL_PREFIX=/usr

# Set this to the path on matte to which release packages should be delivered.
PACKAGE_PATH=/Users/swift-discuss
SOURCEKIT_PACKAGE_PATH=/Users/sourcekit-dev

# Set this to the address to which release announcements should be sent.

# Make sure the variables and directories we expect to exist actually do.
test "$WORKSPACE"
test -d "$WORKSPACE"
test -d "$WORKSPACE/llvm"
test -d "$WORKSPACE/llvm/tools"
test -d "$WORKSPACE/clang"
test -d "$WORKSPACE/swift"
test -d "$WORKSPACE/SourceKit"

# Make sure install-test-script.sh is available alongside us.
INSTALL_TEST_SCRIPT="$(dirname "$0")/install-test-script.sh"
RELEASE_NOTES_TXT="$(dirname "$0")/buildbot-release-notes.txt"

if [ \! -x "$INSTALL_TEST_SCRIPT" ]; then
  echo "Install test script $INSTALL_TEST_SCRIPT is unavailable or not executable!"
  exit 1
fi

if [ \! -f "$RELEASE_NOTES_TXT" ]; then
  echo "Release notes file $RELEASE_NOTES_TXT is unavailable!"
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
      -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
      -DLLVM_TARGETS_TO_BUILD="X86;ARM" \
      -DLLVM_ENABLE_ASSERTIONS="ON" \
      -DCLANG_REPOSITORY_STRING="$CUSTOM_VERSION_NAME" \
      .. &&
    make -j8) || exit 1
fi

# Build Swift.
if [ \! "$SKIP_BUILD_SWIFT" ]; then
  echo "--- Building Swift ---"
  mkdir -p "$WORKSPACE/swift/build"
  (cd "$WORKSPACE/swift/build" &&
    "$CMAKE" -G "Unix Makefiles" \
      -DCMAKE_C_COMPILER="$TOOLCHAIN/usr/bin/clang" \
      -DCMAKE_CXX_COMPILER="$TOOLCHAIN/usr/bin/clang++" \
      -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
      -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
      -DSWIFT_PATH_TO_CLANG_SOURCE="$WORKSPACE/llvm/tools/clang" \
      -DSWIFT_PATH_TO_CLANG_BUILD="$WORKSPACE/llvm/build" \
      -DSWIFT_PATH_TO_LLVM_SOURCE="$WORKSPACE/llvm" \
      -DSWIFT_PATH_TO_LLVM_BUILD="$WORKSPACE/llvm/build" \
      -DSWIFT_MODULE_CACHE_PATH="$WORKSPACE/swift-module-cache" \
      -DSWIFT_RUN_LONG_TESTS="ON" \
      -DLLVM_ENABLE_ASSERTIONS="ON" \
      .. &&
    make -j8) || exit 1
fi

# Build SourceKit.
if [ \! "$SKIP_BUILD_SOURCEKIT" ]; then
  echo "--- Building SourceKit ---"
  mkdir -p "$WORKSPACE/SourceKit/build"
  (cd "$WORKSPACE/SourceKit/build" &&
    "$CMAKE" -G "Unix Makefiles" \
      -DCMAKE_C_COMPILER="$TOOLCHAIN/usr/bin/clang" \
      -DCMAKE_CXX_COMPILER="$TOOLCHAIN/usr/bin/clang++" \
      -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
      -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
      -DSOURCEKIT_PATH_TO_SWIFT_SOURCE="$WORKSPACE/swift" \
      -DSOURCEKIT_PATH_TO_SWIFT_BUILD="$WORKSPACE/swift/build" \
      -DSOURCEKIT_PATH_TO_CLANG_SOURCE="$WORKSPACE/llvm/tools/clang" \
      -DSOURCEKIT_PATH_TO_CLANG_BUILD="$WORKSPACE/llvm/build" \
      -DSOURCEKIT_PATH_TO_LLVM_SOURCE="$WORKSPACE/llvm" \
      -DSOURCEKIT_PATH_TO_LLVM_BUILD="$WORKSPACE/llvm/build" \
      -DSWIFT_MODULE_CACHE_PATH="$WORKSPACE/swift-module-cache" \
      -DLLVM_ENABLE_ASSERTIONS="ON" \
      .. &&
    make -j8) || exit 1
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
    make check-swift) || exit 1
fi

# Run the Swift performance tests.
if [ \! "$SKIP_TEST_SWIFT_PERFORMANCE" ]; then
  # Currently we use the toolchain-specific Clang as our CC under test, because
  # our locally built one might not end up invoking an LD that supports
  # autolinking on older machines. We can reconsider this when it becomes useful
  # to have the C/C++ tests be running using the same LLVM basis as the Swift we
  # are testing.
  echo "--- Running Swift Performance Tests ---"
  export CLANG="$TOOLCHAIN/usr/bin/clang"
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

if [ "$PACKAGE" -a \! "$SKIP_PACKAGE_SWIFT" ]; then
  echo "--- Building Swift Package ---"
  (cd "$WORKSPACE/swift/build" &&
    make -j8 package) || exit 1

  saw_package=
  for package in "$WORKSPACE/swift/build"/swift-*.tar.gz; do
    if [ "$saw_package" ]; then
      echo "More than one package file built!"
      exit 1
    fi
    saw_package=1

    echo "--- Testing $package ---"
    if ! "$INSTALL_TEST_SCRIPT" "$package"; then
      echo "$package failed test!"
      exit 1
    fi

    echo "--- Delivering $package ---"
    cp "$package" "$PACKAGE_PATH" || exit 1

    echo "--- Announcing $package ---"
    package_basename="$(basename "$package")"
    sendmail -r "$PACKAGE_ANNOUNCEMENT_ADDRESS" "$PACKAGE_ANNOUNCEMENT_ADDRESS" <<EOM
To: $PACKAGE_ANNOUNCEMENT_ADDRESS
Subject: Swift package $package_basename now available

A new Swift package is available at
You can download and install it using the command line:

          ~/Downloads
        sudo darwinup install ~/Downloads/$package_basename

where \$OD_USER is your Open Directory username.

We recommend uninstalling any previous Swift packages you have installed
before installing this package. Uninstall as follows:

        darwinup list
        sudo darwinup uninstall \$UUID_FROM_DARWINUP_LIST

When you find bugs in Swift, please report them using the 'Swift' Radar
component.

=== GETTING STARTED WITH SWIFT ===

Once installed, run 'swift' to bring up the interactive prompt:

        swift

Run Swift programs using the '-i' flag:

        swift -i /usr/share/swift/examples/hello.swift

Compile Swift programs to .o files using the '-c' flag. Currently they must
then be linked manually to the swift_stdlib_core library in /usr/lib/swift
using Clang:

        swift -c /usr/share/swift/examples/hello.swift -o hello.o
        clang -o hello hello.o -L/usr/lib/swift -lswift_stdlib_core
        ./hello

Language documentation and examples are installed under /usr/share/swift.

Have fun!

=== RECENT CHANGES ===

$(cat "$RELEASE_NOTES_TXT")

=== KNOWN ISSUES ===

The Swift compiler is under active development and has a number of known
problems. Here are some of the most commonly encountered issues:

* Spectacularly poor error messages: the compiler will often report the
  unhelpful errors "expression does not type-check" or "assignment does not
  type-check", preceded by a debugging dump.

* Run-time errors abort: run-time errors such as an out-of-bounds array access
  are detected by the standard library, which immediately aborts without
  reporting a problem. Moreover, such errors are not trapped in the REPL, and
  will cause the REPL itself to crash.

* Run-time safety: The compiler does not yet check for many error conditions
  and instead compiles and executes code with undefined behavior. In
  particular, functions can fall off the end without returning a value, and
  uninitialized variables will be zero-initialized instead of using a default
  constructor or raising an error.

* Generics support is limited: only basic generic types and functions
  currently work. For example, one cannot create a type that has an instance
  variable whose type is that of a type parameter.

.
EOM
  done

  if [ \! "$saw_package" ]; then
    echo "No package file built!"
    exit 1
  fi
fi

# Run the SourceKit tests.
if [ \! "$SKIP_TEST_SOURCEKIT" ]; then
  export SWIFT="$WORKSPACE/swift/build/bin/swift"
  export SOURCEKITD_TEST="$WORKSPACE/SourceKit/build/bin/sourcekitd-test"
  echo "--- Running SourceKit Tests ---"
  (cd "$WORKSPACE/SourceKit/build" &&
    "$WORKSPACE/llvm/build/bin/llvm-lit" -sv test) || exit 1
fi

if [ "$PACKAGE" -a \! "$SKIP_PACKAGE_SOURCEKIT" ]; then
  echo "--- Building SourceKit Package ---"
  (cd "$WORKSPACE/SourceKit/build" &&
    /bin/sh -ex "$WORKSPACE/SourceKit/utils/buildbot-package-sourcekit.sh") || exit 1

  saw_package=
  for package in "$WORKSPACE/SourceKit/build"/SourceKit-*.tar.gz; do
    if [ "$saw_package" ]; then
      echo "More than one package file built!"
      exit 1
    fi
    saw_package=1

    echo "--- Delivering $package ---"
    cp "$package" "$SOURCEKIT_PACKAGE_PATH" || exit 1

    echo "--- Announcing $package ---"
    package_basename="$(basename "$package")"
    sendmail -r "$SOURCEKIT_PACKAGE_ANNOUNCEMENT_ADDRESS" "$SOURCEKIT_PACKAGE_ANNOUNCEMENT_ADDRESS" <<EOM
To: $SOURCEKIT_PACKAGE_ANNOUNCEMENT_ADDRESS
Subject: SourceKit package $package_basename now available

A new SourceKit package is available at
sftp://matte.apple.com$SOURCEKIT_PACKAGE_PATH/$package_basename .
You can download it using the command line:

        sftp \$OD_USER@matte.apple.com:$SOURCEKIT_PACKAGE_PATH/$package_basename \
          ~/Downloads

where \$OD_USER is your Open Directory username.

EOM
  done

  if [ \! "$saw_package" ]; then
    echo "No package file built!"
    exit 1
  fi
fi
