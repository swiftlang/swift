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
# SKIP_PACKAGE_SWIFT -- set to skip packaging Swift

# The -release flag enables a release build, which will additionally build
# a package if the build and test succeeds.
if [ "$1" = "-release" ]; then
  BUILD_TYPE=RelWithDebInfo
  PACKAGE=1
else
  BUILD_TYPE=Debug
  PACKAGE=
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

# Set this to the address to which release announcements should be sent.

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
      -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
      -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
      -DSWIFT_PATH_TO_CLANG_SOURCE="$WORKSPACE/llvm/tools/clang" \
      -DSWIFT_PATH_TO_CLANG_BUILD="$WORKSPACE/llvm/build" \
      -DSWIFT_PATH_TO_LLVM_SOURCE="$WORKSPACE/llvm" \
      -DSWIFT_PATH_TO_LLVM_BUILD="$WORKSPACE/llvm/build" \
      -DSWIFT_MODULE_CACHE_PATH="$WORKSPACE/swift-module-cache" \
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

if [ "$PACKAGE" -a \! "$SKIP_PACKAGE_SWIFT" ]; then
  echo "--- Building Swift Package ---"
  (cd "$WORKSPACE/swift/build" &&
    make -j package) || exit 1

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

A new Swift package sftp://matte.apple.com/$package_basename
is available. You can download and install it using the command line:

        sftp matte.apple.com:/$package_basename ~/Downloads
        sudo darwinup install ~/Downloads/$package_basename

We recommend uninstalling any previous Swift packages you have installed
before installing this package. Uninstall as follows:

        darwinup list
        sudo darwinup uninstall $UUID_FROM_DARWINUP_LIST

When you find bugs in Swift, please report them using the 'Swift (New Bugs)'
Radar component.

=== GETTING STARTED WITH SWIFT ===

Once installed, run 'swift' to bring up the interactive prompt:

        swift

Run Swift programs using the '-i' flag:

        swift -i /usr/share/swift/examples/hello.swift

Compile Swift programs to .o files using the '-c' flag. Currently they must
then be linked manually to the swift_stdlib library in /usr/lib/swift using
Clang:

        swift -c /usr/share/swift/examples/hello.swift -o hello.o
        clang -o hello hello.o -L/usr/lib/swift -lswift_stdlib
        ./hello

Language documentation and examples are installed under /usr/share/swift.

Have fun!

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

* Protocol support is limited: creating a variable of protocol type without
  initializing it results in a variable that is unusable; any attempt to query
  or assign it will cause a crash. Additionally, Objective-C protocols are not
  supported.

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
