// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

func foo(_ f: @autoclosure () -> Int) {}
func foo(_ f: () -> Int) {}

func bar(_ f: () throws -> Int) {}
func bar(_ f: () -> Int) {}

func baz(a1: @autoclosure () -> Int,
         a2: () -> Int,
         b1: () throws -> Int,
         b2: () -> Int) {
  // CHECK: function_ref @_T012rdar371606793fooySiyXKF
  foo(a1)
  // CHECK: function_ref @_T012rdar371606793fooySiycF
  foo(a2)
  // CHECK: function_ref @_T012rdar371606793barySiyKcF
  bar(b1)
  // CHECK: function_ref @_T012rdar371606793barySiycF
  bar(b2)
}
