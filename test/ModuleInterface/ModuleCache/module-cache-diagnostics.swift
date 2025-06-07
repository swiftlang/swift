// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)
// RUN: export SWIFT_BACKTRACE=

// rdar://88830153
// https://github.com/apple/swift/issues/58134
// This test is flaky on Windows.
// UNSUPPORTED: OS=windows-msvc

// Setup builds a module TestModule that depends on OtherModule and LeafModule (built from other.swift and leaf.swift).
// During setup, input and intermediate mtimes are all set to a constant 'old' time (long in the past).
//
// We then revert LeafModule.swiftinterface to have a warning-provoking entry in it, and check that we get no diagnostic.
//
// We then modify LeafModule.swiftinterface to have an error in it, and check that we get a diagnostic and failure.
//
// We then modify LeafModule.swiftinterface to have an error in an @inlinable function body, and check we get a diagnostic and failure.
//
// Setup phase 1: Write input files.
//
// RUN: echo 'public func LeafFunc() -> Int { return 10; }' >%t/leaf.swift
//
// RUN: echo 'import LeafModule' >%t/other.swift
// RUN: echo 'public func OtherFunc() -> Int { return LeafFunc(); }' >>%t/other.swift
//
//
// Setup phase 2: build modules, pushing timestamps of inputs and intermediates into the past as we go.
//
// RUN: %{python} %S/Inputs/make-old.py %t/leaf.swift %t/other.swift
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -emit-module-interface-path %t/LeafModule.swiftinterface -module-name LeafModule %t/leaf.swift -emit-module -o /dev/null
// RUN: %{python} %S/Inputs/make-old.py %t/LeafModule.swiftinterface
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/modulecache -emit-module-interface-path %t/OtherModule.swiftinterface -module-name OtherModule %t/other.swift -emit-module -o /dev/null
// RUN: %{python} %S/Inputs/make-old.py %t/modulecache/LeafModule-*.swiftmodule %t/OtherModule.swiftinterface
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/modulecache -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: %{python} %S/Inputs/make-old.py %t/modulecache/OtherModule-*.swiftmodule
//
//
// Actual test: add an inlinable func with an unused var to LeafModule.swiftinterface (which should emit a warning); check we do get a rebuild, but no warning. (For astooscopelookup testing, must filter out that warning; see the sed command below.)
//
// RUN: %{python} %S/Inputs/check-is-old.py %t/OtherModule.swiftinterface %t/LeafModule.swiftinterface
// RUN: %{python} %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: cp %t/LeafModule.swiftinterface %t/LeafModule.swiftinterface.backup
// RUN: echo "@inlinable func foo() { var x = 10 }" >>%t/LeafModule.swiftinterface
// RUN: %{python} %S/Inputs/make-old.py %t/LeafModule.swiftinterface
// RUN: %{python} %S/Inputs/check-is-old.py %t/LeafModule.swiftinterface
// RUN: rm %t/TestModule.swiftmodule
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/modulecache -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s  2>&1 | sed '/WARNING: TRYING Scope exclusively/d' >%t/warn.txt
// RUN: %{python} %S/Inputs/check-is-new.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// "check warn.txt exists and is empty"
// RUN: test -e %t/warn.txt -a ! -s %t/warn.txt
//
//
// Next test: make LeafModule.swiftinterface import NotAModule (which should emit an error); check we do not get a rebuild, do get an error.
//
// RUN: %{python} %S/Inputs/make-old.py %t/modulecache/*.swiftmodule %t/*.swiftinterface
// RUN: %{python} %S/Inputs/check-is-old.py %t/OtherModule.swiftinterface %t/LeafModule.swiftinterface
// RUN: %{python} %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: echo "import NotAModule" >>%t/LeafModule.swiftinterface
// RUN: %{python} %S/Inputs/make-old.py %t/LeafModule.swiftinterface
// RUN: %{python} %S/Inputs/check-is-old.py %t/LeafModule.swiftinterface
// RUN: rm %t/TestModule.swiftmodule
// RUN: not %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/modulecache -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s >%t/err.txt 2>&1
// RUN: %{python} %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: %FileCheck %s -check-prefix=CHECK-ERROR <%t/err.txt
// CHECK-ERROR: LeafModule.swiftinterface:8:8: error: no such module 'NotAModule'
// CHECK-ERROR: OtherModule.swiftinterface:5:8: error: failed to build module 'LeafModule' for importation due to the errors above; the textual interface may be broken by project issues or a compiler bug
// CHECK-ERROR: module-cache-diagnostics.swift:{{[0-9]+}}:8: error: failed to build module 'OtherModule' for importation due to the errors above; the textual interface may be broken by project issues or a compiler bug
//
//
// Next test: same as above, but with a .dia file
//
// RUN: %{python} %S/Inputs/check-is-old.py %t/OtherModule.swiftinterface %t/LeafModule.swiftinterface
// RUN: %{python} %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: not %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/modulecache -serialize-diagnostics -serialize-diagnostics-path %t/err.dia -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: %{python} %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: c-index-test -read-diagnostics %t/err.dia 2>&1 | %FileCheck %s -check-prefix=CHECK-ERROR
//
//
// Next test: add an inlinable function body with an cannot find to LeafModule.swiftinterface; check we do not get a rebuild and report the additional error correctly.
//
// RUN: mv %t/LeafModule.swiftinterface.backup %t/LeafModule.swiftinterface
// RUN: echo "@inlinable func bar() { var x = unresolved }" >>%t/LeafModule.swiftinterface
// RUN: %{python} %S/Inputs/make-old.py %t/LeafModule.swiftinterface
// RUN: %{python} %S/Inputs/check-is-old.py %t/OtherModule.swiftinterface %t/LeafModule.swiftinterface
// RUN: %{python} %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: not %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/modulecache -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s >%t/err-inline.txt 2>&1
// RUN: %{python} %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: %FileCheck %s -check-prefix=CHECK-ERROR-INLINE <%t/err-inline.txt
// CHECK-ERROR-INLINE: LeafModule.swiftinterface:7:33: error: cannot find 'unresolved' in scope
// CHECK-ERROR-INLINE: OtherModule.swiftinterface:5:8: error: failed to build module 'LeafModule' for importation due to the errors above; the textual interface may be broken by project issues or a compiler bug
// CHECK-ERROR-INLINE: module-cache-diagnostics.swift:{{[0-9]+}}:8: error: failed to build module 'OtherModule' for importation due to the errors above; the textual interface may be broken by project issues or a compiler bug
//
//
// Next test: same as above, but with a .dia file
//
// RUN: %{python} %S/Inputs/check-is-old.py %t/OtherModule.swiftinterface %t/LeafModule.swiftinterface
// RUN: %{python} %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: not %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/modulecache -serialize-diagnostics -serialize-diagnostics-path %t/err-inline.dia -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: %{python} %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: c-index-test -read-diagnostics %t/err-inline.dia 2>&1 | %FileCheck %s -check-prefix=CHECK-ERROR-INLINE

import OtherModule

public func TestFunc() {
    print(OtherFunc())
}

