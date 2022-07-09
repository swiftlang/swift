// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface)
// RUN: %FileCheck %s < %t.swiftinterface

// RUN: %target-swift-emit-module-interface(%t.multifile.swiftinterface) %s %S/Inputs/other.swift
// RUN: %target-swift-typecheck-module-from-interface(%t.multifile.swiftinterface)
// RUN: %FileCheck -check-prefix CHECK -check-prefix CHECK-MULTI-FILE %s < %t.multifile.swiftinterface

// CHECK: public func verySimpleFunction(){{$}}
public func verySimpleFunction() {}

// CHECK: public func ownership(_ x: __shared Swift.AnyObject){{$}}
public func ownership(_ x: __shared AnyObject) {}

// CHECK-MULTI-FILE: public func otherFileFunction(){{$}}
