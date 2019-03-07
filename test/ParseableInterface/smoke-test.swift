// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path - %s | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path - %s %S/Inputs/other.swift | %FileCheck -check-prefix CHECK -check-prefix CHECK-MULTI-FILE %s

// CHECK: public func verySimpleFunction(){{$}}
public func verySimpleFunction() {}

// CHECK: public func ownership(_ x: __shared AnyObject){{$}}
public func ownership(_ x: __shared AnyObject) {}

// CHECK-MULTI-FILE: public func otherFileFunction(){{$}}
