// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/SomeProtocol.swift -module-name SomeProtocol -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name Something -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name Something -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Something.symbols.json

import protocol SomeProtocol.P

public struct MyStruct: P {
  public func foo() {}
}

// CHECK: "kind": "conformsTo",
// CHECK-NEXT: "source": "s:9Something8MyStructV",
// CHECK-NEXT: "target": "s:12SomeProtocol1PP",
