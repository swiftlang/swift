:orphan:

.. @raise litre.TestsAreMissing

=============
Testing Swift
=============

This document describes how we test the Swift compiler, the Swift runtime, and
the Swift standard library.

Testing approaches
==================

We use multiple approaches to test the Swift toolchain.

* LLVM lit-based testsuites for the compiler, runtime and the standard library.

* A selection of open source projects written in Swift.

The LLVM lit-based testsuite
============================

**Purpose**: primary testsuites for the Swift toolchain.

**Contents**: Functional and regression tests for all toolchain components.

**Run by**:

* Engineers and contributors are expected to run tests from these testsuites
  locally before committing.  (Usually on a single platform, and not necessarily
  all tests.)

* Buildbots run all tests, on all supported platforms.

Running the LLVM lit-based testsuite
------------------------------------

You can run Swift tests using the ``build-script``, or, alternatively, using
these targets in the build directory:

* ``check-swift``

  Runs tests from the ``${SWIFT_SOURCE_ROOT}/test`` directory.

* ``check-swift-validation``

  Runs tests from the ``${SWIFT_SOURCE_ROOT}/validation-test`` directory.

* ``check-swift-all``

  Runs all tests.

For day-to-day work on the Swift compiler, using check-swift should be
sufficient.  The buildbot runs validation tests, so if those are accidentally
broken, it should not go unnoticed.

Before committing a large change to a compiler (especially a language change),
or API changes to the standard library, it is recommended to run validation
test suite.

For every target above, there are variants for different optimizations:

* the target itself (e.g., ``check-swift``) -- runs execution tests in
  ``-Onone`` mode;

* the target with ``-optimize`` suffix (e.g., ``check-swift-optimize``) -- runs
  execution tests in ``-O`` mode; This target will only run tests marked as
  ``executable_test``.

* the target with ``-optimize-unchecked`` suffix (e.g.,
  ``check-swift-optimize-unchecked``) -- runs execution tests in
  ``-Ounchecked`` mode. This target will only run tests marked as
  ``executable_test``.

If you need to manually run certain tests, you can invoke LLVM's lit.py script
directly. For example::

    % ${LLVM_SOURCE_ROOT}/utils/lit/lit.py -sv ${SWIFT_BUILD_ROOT}/test-iphonesimulator-i386/Parse/

This runs the tests in the test/Parse/ directory targeting the 32-bit iOS
Simulator. The ``-sv`` options give you a nice progress bar and only show you
output from the tests that fail.

One downside of using this form is that you're appending relative paths from
the source directory to the test directory in your build directory. (That is,
there may not actually be a directory named 'Parse' in
'test-iphonesimulator-i386/'; the invocation works because there is one in the
source 'test/' directory.) There is a more verbose form that specifies the
testing configuration explicitly, which then allows you to test files
regardless of location.

::

    % ${LLVM_SOURCE_ROOT}/utils/lit/lit.py -sv --param swift_site_config=${SWIFT_BUILD_ROOT}/test-iphonesimulator-i386/lit.site.cfg ${SWIFT_SOURCE_ROOT}/test/Parse/

For more complicated configuration, copy the invocation from one of the build
targets mentioned above and modify it as necessary. lit.py also has several
useful features, like timing tests and providing a timeout. Check these features
out with ``lit.py -h``.

Writing tests
-------------

General guidelines
^^^^^^^^^^^^^^^^^^

When adding a new testcase, try to find an existing test file focused on the
same topic rather than starting a new test file.  There is a fixed runtime cost
for every test file.  On the other hand, avoid dumping new tests in a file that
is only remotely related to the purpose of the new tests.

Don't limit a test to a certain platform or hardware configuration just because
this makes the test slightly easier to write.  This sometimes means a little
bit more work when adding the test, but the payoff from the increased testing
is significant.  We heavily rely on portable tests to port Swift to other
platforms.

Avoid using unstable language features in tests which test something else (for
example, avoid using an unstable underscored attribute when another
non-underscored attribute would work).

Avoid using arbitrary implementation details of the standard library.  Always
prefer to define types locally in the test, if feasible.

Avoid purposefully shadowing names from the standard library, this makes the
test extremely confusing (if nothing else, to understand the intent --- was the
compiler bug triggered by this shadowing?)  When reducing a compiler testcase
from the standard library source, rename the types and APIs in the testcase to
differ from the standard library APIs.

In IRGen, SILGen and SIL tests, avoid using platform-dependent implementation
details of the standard library (unless doing so is point of the test).
Platform-dependent details include:

* ``Int`` (use integer types with explicit types instead).

* Layout of ``String``, ``Array``, ``Dictionary``, ``Set``.  These differ
  between platforms that have Objective-C interop and those that don't.

Unless testing the standard library, avoid using arbitrary standard library
types and APIs, even if it is very convenient for you to do so in your tests.
Using the more common APIs like ``Array`` subscript or ``+`` on ``IntXX`` is
acceptable.  This is important because you can't rely on the full standard
library being available.  The long-term plan is to introduce a mock, minimal
standard library that only has a very basic set of APIs.

If you write an executable test please add ``REQUIRES: executable_test`` to the
test.

Substitutions in lit tests
^^^^^^^^^^^^^^^^^^^^^^^^^^

Substitutions that start with ``%target`` configure the compiler for building
code for the target that is not the build machine:

* ``%target-parse-verify-swift``: parse and type check the current Swift file
  for the target platform and verify diagnostics, like ``swift -parse -verify
  %s``.

  Use this substitution for testing semantic analysis in the compiler.

* ``%target-swift-frontend``: run ``swift -frontend`` for the target.

  Use this substitution (with extra arguments) for tests that don't fit any
  other pattern.

* ``%target-swift-frontend(mock-sdk:`` *mock sdk arguments* ``)`` *other
  arguments*: like ``%target-swift-frontend``, but allows to specify command
  line parameters (typically ``-sdk`` and ``-I``) to use a mock SDK and SDK
  overlay that would take precedence over the target SDK.

* ``%target-build-swift``: compile and link a Swift program for the target.

  Use this substitution only when you intend to run the program later in the
  test.

* ``%target-run-simple-swift``: build a one-file Swift program and run it on
  the target machine.

  Use this substitution for executable tests that don't require special
  compiler arguments.

  Add ``REQUIRES: executable_test`` to the test.

* ``%target-run-stdlib-swift``: like ``%target-run-simple-swift`` with
  ``-parse-stdlib -Xfrontend -disable-access-control``.

  This is sometimes useful for testing the Swift standard library.

  Add ``REQUIRES: executable_test`` to the test.

* ``%target-repl-run-simple-swift``: run a Swift program in a REPL on the
  target machine.

* ``%target-run``: run a command on the target machine.

  Add ``REQUIRES: executable_test`` to the test.

* ``%target-jit-run``: run a Swift program on the target machine using a JIT
  compiler.

* ``%target-swiftc_driver``: FIXME

* ``%target-sil-opt``: run ``sil-opt`` for the target.

* ``%target-sil-extract``: run ``sil-extract`` for the target.

* ``%target-swift-ide-test``: run ``swift-ide-test`` for the target.

* ``%target-swift-ide-test(mock-sdk:`` *mock sdk arguments* ``)`` *other
  arguments*: like ``%target-swift-ide-test``, but allows to specify command
  line parameters to use a mock SDK.

* ``%target-swiftc_driver``: FIXME.

* ``%target-swift-autolink-extract``: run ``swift-autolink-extract`` for the
  target to extract its autolink flags on platforms that support them (when the
  autolink-extract feature flag is set)

* ``%target-clang``: run the system's ``clang++`` for the target.

  If you want to run the ``clang`` executable that was built alongside 
  Swift, use ``%clang`` instead.

* ``%target-ld``: run ``ld`` configured with flags pointing to the standard
  library directory for the target.

* ``%target-cc-options``: the clang flags to setup the target with the right
  architecture and platform version.

Always use ``%target-*`` substitutions unless you have a good reason.  For
example, an exception would be a test that checks how the compiler handles
mixing module files for incompatible platforms (that test would need to compile
Swift code for two different platforms that are known to be incompatible).

When you can't use ``%target-*`` substitutions, you can use:

* ``%swift_driver_plain``: FIXME.
* ``%swiftc_driver_plain``: FIXME.
* ``%swift_driver``: FIXME.
* ``%swiftc_driver``: FIXME.
* ``%sil-opt``: FIXME.
* ``%sil-extract``: FIXME.
* ``%lldb-moduleimport-test``: FIXME.
* ``%swift-ide-test_plain``: FIXME.
* ``%swift-ide-test``: FIXME.
* ``%llvm-opt``: FIXME.
* ``%swift``: FIXME.
* ``%clang-include-dir``: FIXME.
* ``%clang-importer-sdk``: FIXME.

Other substitutions:

* ``%leaks-runner``: FIXME.
* ``%clang_apinotes``: FIXME.
* ``%clang``: FIXME.
* ``%target-triple``: FIXME, possible values.
* ``%target-cpu``: FIXME, possible values.
* ``%target-os``: FIXME, possible values.
* ``%target-object-format``: the platform's object format (elf, macho, coff).
* ``%target-runtime``: the platform's Swift runtime (objc, native).
* ``%target-ptrsize``: the pointer size of the target (32, 64).
* ``%sdk``: FIXME.
* ``%gyb``: FIXME.

* ``%platform-module-dir``: absolute path of the directory where the standard
  library module file for the target platform is stored.  For example,
  ``/.../lib/swift/macosx``.

* ``%platform-sdk-overlay-dir``: absolute path of the directory where the SDK
  overlay module files for the target platform are stored.

* ``%target-swiftmodule-name`` and ``%target-swiftdoc-name``: the basename of
  swiftmodule and swiftdoc files for a framework compiled for the target (for
  example, ``arm64.swiftmodule`` and ``arm64.swiftdoc``).

* ``%target-sdk-name``: only for Apple platforms: ``xcrun``-style SDK name
  (``macosx``, ``iphoneos``, ``iphonesimulator``).

When writing a test where output (or IR, SIL) depends on the bitness of the
target CPU, use this pattern::

  // RUN: %target-swift-frontend ... | FileCheck --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize %s

  // CHECK: common line
  // CHECK-32: only for 32-bit
  // CHECK-64: only for 64-bit

  // FileCheck does a single pass for a combined set of CHECK lines, so you can
  // do this:
  //
  // CHECK: define @foo() {
  // CHECK-32: integer_literal $Builtin.Int32, 0
  // CHECK-64: integer_literal $Builtin.Int64, 0

When writing a test where output (or IR, SIL) depends on the target CPU itself,
use this pattern::

  // RUN: %target-swift-frontend ... | FileCheck --check-prefix=CHECK --check-prefix=CHECK-%target-cpu %s

  // CHECK: common line
  // CHECK-i386:   only for i386
  // CHECK-x86_64: only for x86_64
  // CHECK-armv7:  only for armv7
  // CHECK-arm64:  only for arm64

Features for ``REQUIRES`` and ``XFAIL``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

FIXME: full list.

* ``swift_ast_verifier``: present if the AST verifier is enabled in this build.

When writing a test specific to x86, if possible, prefer ``REQUIRES:
CPU=i386_or_x86_64`` to ``REQUIRES: CPU=x86_64``.

``swift_test_mode_optimize[_unchecked|none]`` and
``swift_test_mode_optimize[_unchecked|none]_<CPUNAME>`` to specify a test mode
plus cpu configuration.

``optimized_stdlib_<CPUNAME>``` to specify a optimized stdlib plus cpu
configuration.

Feature ``REQUIRES: executable_test``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This feature marks an executable test. The test harness makes this feature
generally available. It can be used to restrict the set of tests to run.

StdlibUnittest
^^^^^^^^^^^^^^

Tests accept command line parameters, run StdlibUnittest-based test binary
with ``--help`` for more information.

Testing memory management in execution tests
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In execution tests, memory management testing should be performed
using local variables enclosed in a closure passed to the standard
library ``autoreleasepool`` function. For example::

  // A counter that's decremented by Canary's deinitializer.
  var CanaryCount = 0

  // A class whose instances increase a counter when they're destroyed.
  class Canary {
    deinit { ++CanaryCount }
  }

  // Test that a local variable is correctly released before it goes out of
  // scope.
  CanaryCount = 0
  autoreleasepool {
    let canary = Canary()
  }
  assert(CanaryCount == 1, "canary was not released")

Memory management tests should be performed in a local scope because Swift does
not guarantee the destruction of global variables. Code that needs to
interoperate with Objective-C may put references in the autorelease pool, so
code that uses an ``if true {}`` or similar no-op scope instead of
``autoreleasepool`` may falsely report leaks or fail to catch overrelease bugs.
If you're specifically testing the autoreleasing behavior of code, or do not
expect code to interact with the Objective-C runtime, it may be OK to use ``if
true {}``, but those assumptions should be commented in the test.
