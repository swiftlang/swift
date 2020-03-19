// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -emit-ir %s

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
}

func crashCaller() {
  let fType = ResilientBool(b: false)
  let fooImp = StructWithFunc()
  let badStruct = ProtAndResilStruct(fType: fType, fooImp: fooImp)
  badStruct.crash()
}

crashCaller()
