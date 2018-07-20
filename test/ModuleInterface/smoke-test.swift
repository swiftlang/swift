// RUN: %target-swift-frontend -emit-interface-path - -emit-module -o /dev/null %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-interface-path - -emit-module -o /dev/null %s %S/Inputs/other.swift | %FileCheck -check-prefix CHECK-MULTI-FILE %s

// CHECK: public func verySimpleFunction(){{$}}
// CHECK-MULTI-FILE: public func verySimpleFunction(){{$}}
public func verySimpleFunction() {}

// CHECK-MULTI-FILE: public func otherFileFunction(){{$}}
