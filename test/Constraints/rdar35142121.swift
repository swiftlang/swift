// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

func foo<T>(_ a: T) -> Int {
  return 0
}

func foo(_ a: (Int) -> (Int)) -> Int {
  return 42
}

// CHECK: function_ref @$s12rdar351421213fooyS3iXEF : $@convention(thin) (@noescape @callee_guaranteed (Int) -> Int) -> Int
let _ = foo({ (a: Int) -> Int in a + 1 })
