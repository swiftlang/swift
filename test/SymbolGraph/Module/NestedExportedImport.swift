// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-generated)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/A.swift -module-name A -emit-module -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend %t/B.swift -module-name B -emit-module -emit-module-path %t/B.swiftmodule -I %t
// RUN: %target-swift-frontend %t/C.swift -module-name C -emit-module -emit-module-path %t/C.swiftmodule -I %t \
// RUN:     -emit-symbol-graph -emit-symbol-graph-dir %t
// RUN: %target-swift-symbolgraph-extract -module-name C -I %t -output-dir \
// RUN:     %t/module-generated/ -experimental-allowed-reexported-modules=A,B
// RUN: %FileCheck %s --input-file %t/C.symbols.json
// RUN: %FileCheck %s --input-file %t/module-generated/C.symbols.json

//--- A.swift
public struct A {}
public func AFunc() -> Void {}

//--- B.swift
@_exported import A
public struct B {}
public func BFunc() -> Void {}

//--- C.swift
@_exported import B
public struct C {}
public func CFunc() -> Void {}

// CHECK-DAG: "precise":"s:1CAAV"
// CHECK-DAG: "precise":"s:1BAAV"
// CHECK-DAG: "precise":"s:1AAAV"
