// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)
//
// Test will build a module TestModule that depends LeafModule (built from leaf.swift).
//
// RUN: echo 'public func LeafFunc() -> Int { return 10; }' >%t/leaf.swift
//
// RUN: %target-swift-frontend -I %t -emit-parseable-module-interface-path %t/LeafModule.swiftinterface -module-name LeafModule %t/leaf.swift -emit-module -o /dev/null
// RUN: test -f %t/LeafModule.swiftinterface
// RUN: %FileCheck %s -check-prefix=CHECK-LEAFINTERFACE <%t/LeafModule.swiftinterface
// CHECK-LEAFINTERFACE: {{swift-interface-format-version: [0-9\\.]+}}
// CHECK-LEAFINTERFACE: LeafFunc
//
// Break LeafModule's version number
// RUN: sed -i.prev -e 's/swift-interface-format-version:.*/swift-interface-format-version: 9999.999/' %t/LeafModule.swiftinterface
//
// Try to build TestModule into a .swiftmodule explicitly using LeafModule via LeafModule.swiftinterface, but fail because version mismatch in LeafModule.swiftinterface.
//
// RUN: not %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-module  -o %t/TestModule.swiftmodule -module-name TestModule %s >%t/err.txt 2>&1
// RUN: test ! -f %t/TestModule.swiftmodule
// RUN: test ! -f %t/modulecache/LeafModule-*.swiftmodule
// RUN: %FileCheck %s -check-prefix=CHECK-ERR <%t/err.txt
// CHECK-ERR: {{error: unsupported version of parseable module interface '.*/LeafModule.swiftinterface': '9999.999'}}
// CHECK-ERR: error: no such module 'LeafModule

import LeafModule

public func TestFunc() {
    print(LeafFunc())
}
