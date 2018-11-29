// REQUIRES: rdar46073729
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)
//
// Setup builds a module TestModule that depends on OtherModule and LeafModule (built from other.swift and leaf.swift).
// During setup, input and intermediate mtimes are all set to a constant 'old' time (long in the past).
//
// We then revert LeafModule.swiftinterface to have a warning-provoking entry in it, and check that we get no diagnostic.
//
// We then modify LeafModule.swiftinterface to have an error in it, and check that we get a diagnostic and failure.
//
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
// RUN: %S/Inputs/make-old.py %t/leaf.swift %t/other.swift
// RUN: %target-swift-frontend -I %t -emit-parseable-module-interface-path %t/LeafModule.swiftinterface -module-name LeafModule %t/leaf.swift -emit-module -o /dev/null
// RUN: %S/Inputs/make-old.py %t/LeafModule.swiftinterface
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-parseable-module-interface-path %t/OtherModule.swiftinterface -module-name OtherModule %t/other.swift -emit-module -o /dev/null
// RUN: %S/Inputs/make-old.py %t/modulecache/LeafModule-*.swiftmodule %t/OtherModule.swiftinterface
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: %S/Inputs/make-old.py %t/modulecache/OtherModule-*.swiftmodule
//
//
// Actual test: add an inlinable func with an unused var to LeafModule.swiftinterface (which should emit a warning); check we do get a rebuild, but no warning.
//
// RUN: %S/Inputs/check-is-old.py %t/OtherModule.swiftinterface %t/LeafModule.swiftinterface
// RUN: %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: echo "@inlinable func foo() { var x = 10 }" >>%t/LeafModule.swiftinterface
// RUN: %S/Inputs/make-old.py %t/LeafModule.swiftinterface
// RUN: %S/Inputs/check-is-old.py %t/LeafModule.swiftinterface
// RUN: rm %t/TestModule.swiftmodule
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s >%t/warn.txt 2>&1
// RUN: %S/Inputs/check-is-new.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// "check warn.txt exists and is empty"
// RUN: test -e %t/warn.txt -a ! -s %t/warn.txt
//
//
// Next test: make LeafModule.swiftinterface import NotAModule (which should emit an error); check we do not get a rebuild, do get an error.
//
// RUN: %S/Inputs/make-old.py %t/modulecache/*.swiftmodule %t/*.swiftinterface
// RUN: %S/Inputs/check-is-old.py %t/OtherModule.swiftinterface %t/LeafModule.swiftinterface
// RUN: %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: echo "import NotAModule" >>%t/LeafModule.swiftinterface
// RUN: %S/Inputs/make-old.py %t/LeafModule.swiftinterface
// RUN: %S/Inputs/check-is-old.py %t/LeafModule.swiftinterface
// RUN: rm %t/TestModule.swiftmodule
// RUN: not %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s >%t/err.txt 2>&1
// RUN: %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: %FileCheck %s -check-prefix=CHECK-ERROR <%t/err.txt
// CHECK-ERROR: LeafModule.swiftinterface:6:8: error: no such module 'NotAModule'
// CHECK-ERROR: OtherModule.swiftinterface:4:8: error: no such module 'LeafModule'
//
//
// Next test: same as above, but with a .dia file
//
// RUN: %S/Inputs/check-is-old.py %t/OtherModule.swiftinterface %t/LeafModule.swiftinterface
// RUN: %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: not %target-swift-frontend -I %t -module-cache-path %t/modulecache -serialize-diagnostics -serialize-diagnostics-path %t/err.dia -enable-parseable-module-interface -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: %S/Inputs/check-is-old.py %t/modulecache/OtherModule-*.swiftmodule %t/modulecache/LeafModule-*.swiftmodule
// RUN: c-index-test -read-diagnostics %t/err.dia 2>&1 | %FileCheck %s -check-prefix=CHECK-ERROR

import OtherModule

public func TestFunc() {
    print(OtherFunc())
}

