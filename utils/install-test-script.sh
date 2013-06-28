#!/bin/sh -x

# Smoke tests a Swift installation package.
# Set these to the paths of the OS X SDK and toolchain.
SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk
TOOLCHAIN=/Applications/Xcode.app/Contents/Developer/Toolchains/OSX10.9.xctoolchain

# FIXME: OSX 10.9 bug <rdar://problem/13228632>: TMPDIR doesn't get set sometimes.
if [ ! "$TMPDIR" ]; then
  TMPDIR=/tmp
fi

# Wipe out stale module caches.
find "$TMPDIR" -name "*.pcm" -exec rm '{}' ';'
find /var/tmp -name "*.pcm" -exec rm '{}' ';'
find /tmp -name "*.pcm" -exec rm '{}' ';'
find "$(getconf DARWIN_USER_CACHE_DIR)" -name "*.pcm" -exec rm '{}' ';'

# The package name should be given as the first argument.
PACKAGE_NAME="$1"

if [ \! "$PACKAGE_NAME" ]; then
  echo "No package name given! Usage: $0 package.tar.gz"
  exit 1
elif [ \! -f "$PACKAGE_NAME" ]; then
  echo "Package $PACKAGE_NAME does not exist!"
  exit 1
fi

# We use a sudoable script to darwinup install and uninstall the
# package.
INSTALL_PACKAGE_HELPER="$(dirname "$0")/install-package-helper.sh"

if [ \! -x "$INSTALL_PACKAGE_HELPER" ]; then
  echo "Unable to find package installer helper $INSTALL_PACKAGE_HELPER!"
  exit 1
fi

# Install the package.
if ! sudo -n "$INSTALL_PACKAGE_HELPER" install "$PACKAGE_NAME"; then
  echo "Failed to install package!"
  exit 1
fi

#
# Do tests.
#

# Ensure that basic REPL stuff works.
# FIXME: REPL bug--when stdout is redirected but stderr is a terminal, no
# output appears on stdout.
RESULT=0

if ! /usr/bin/swift -repl 2>"$TMPDIR/test_repl_1_err_$$" >"$TMPDIR/test_repl_1_$$" <<REPL
println("Hello world")
REPL
then
  echo "swift failed in REPL!"
  RESULT=1
elif [ "$(cat "$TMPDIR/test_repl_1_$$")" != "Hello world" ]; then
  echo "REPL did not output expected result!"
  RESULT=1
fi

if ! /usr/bin/swift -repl 2>"$TMPDIR/test_repl_2_err_$$" >"$TMPDIR/test_repl_2_$$" <<REPL
import Foundation
println("Hello world" as NSString)
REPL
then
  echo "swift failed in REPL with SDK!"
  RESULT=1
elif [ "$(cat "$TMPDIR/test_repl_2_$$")" != "Hello world" ]; then
  echo "REPL with SDK did not output expected result!"
  RESULT=1
fi

# Ensure that we can compile and link a Swift program and that the Swift
# libs are all findable by -l flags.
cat >"$TMPDIR/test_compile_$$.swift" <<TEST_COMPILE
import Foundation
var s : NSString = "world"
print("Hello ")
println(s)
TEST_COMPILE

if ! /usr/bin/swift -c "$TMPDIR/test_compile_$$.swift" \
  -o "$TMPDIR/test_compile_$$.o"
then
  echo "Failed to compile Swift program!"
  RESULT=1
elif ! "$TOOLCHAIN/usr/bin/clang" "$TMPDIR/test_compile_$$.o" \
  -o "$TMPDIR/test_compile_$$" \
  -L/usr/lib/swift \
  -framework Cocoa -lswift_stdlib_core -lswiftFoundation -lswiftObjectiveC \
  -lswiftAppKit
then
  echo "Failed to link Swift program!"
  RESULT=1
elif [ "$($TMPDIR/test_compile_$$)" != "Hello world" ]; then
  echo "Running Swift program did not give expected result!"
  RESULT=1
# Ensure that we can link a Swift program even if -isysroot points to an SDK.
elif ! "$TOOLCHAIN/usr/bin/clang" "$TMPDIR/test_compile_$$.o" \
  -isysroot "$SYSROOT" \
  -o "$TMPDIR/test_compile_2_$$" \
  -L/usr/lib/swift \
  -framework Cocoa -lswift_stdlib_core -lswiftFoundation -lswiftObjectiveC \
  -lswiftAppKit
then
  echo "Failed to link Swift program with -isysroot!"
  RESULT=1
elif [ "$($TMPDIR/test_compile_2_$$)" != "Hello world" ]; then
  echo "Running Swift program linked with -isysroot did not give expected result!"
  RESULT=1
fi



#
# Done with tests.
#

# Uninstall the package.
if ! sudo -n "$INSTALL_PACKAGE_HELPER" uninstall "$PACKAGE_NAME"; then
  echo "Failed to uninstall package!"
  exit 1
fi

exit "$RESULT"
