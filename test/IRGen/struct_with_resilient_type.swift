// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -emit-ir %s | %FileCheck %s

// REQUIRES: CPU=x86_64

import resilient_struct

struct StructWithFunc {
  func foo(ptr: @escaping () -> Void) {
  }
}

struct ProtAndResilStruct {
  let foundationType: ResilientBool
  
  let fooImp: StructWithFunc
  
  init(fType: ResilientBool, fooImp: StructWithFunc) {
    self.foundationType = fType
    self.fooImp = fooImp
  }
  
  func bar() {
  }
  
  func crash() {
    fooImp.foo(ptr: bar)
  }
// CHECK-LABEL: define{{.*}} @"$S26struct_with_resilient_type18ProtAndResilStructV3baryyFTc"(%T26struct_with_resilient_type18ProtAndResilStructV* noalias nocapture)
// CHECK:   %flags.alignmentMask = and i64 %flags, 255
// CHECK: [[XOR_ALIGN:%.*]] = xor i64 %flags.alignmentMask, -1
// CHECK: [[INIT_OFFSET:%.*]] = add i64 16, %flags.alignmentMask
// CHECK: [[T0:%.*]] = and i64 [[INIT_OFFSET]], [[XOR_ALIGN]]
// CHECK: [[T1:%.*]] = add i64 [[T0]], %size
// CHECK: [[ALIGN:%.*]] = or i64 7, %flags.alignmentMask
}

func crashCaller() {
  let fType = ResilientBool(b: false)
  let fooImp = StructWithFunc()
  let badStruct = ProtAndResilStruct(fType: fType, fooImp: fooImp)
  badStruct.crash()
}

crashCaller()
