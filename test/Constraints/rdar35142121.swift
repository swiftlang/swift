// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

func foo<T>(_ a: T) -> Int {
  return 0
}

func foo(_ a: (Int) -> (Int)) -> Int {
  return 42
}

// CHECK: function_ref @_T012rdar351421213fooS3icF : $@convention(thin) (@owned @noescape @callee_guaranteed (Int) -> Int) -> Int
let _ = foo({ (a: Int) -> Int in a + 1 })
