// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

func foo(_ f: @autoclosure () -> Int) {}
func foo(_ f: () -> Int) {}

func bar(_ f: () throws -> Int) {}
func bar(_ f: () -> Int) {}

func baz(a1: @autoclosure () -> Int,
         a2: () -> Int,
         b1: () throws -> Int,
         b2: () -> Int) {
  // CHECK: function_ref @$s12rdar371606793fooyySiyXKF
  foo(a1)
  // CHECK: function_ref @$s12rdar371606793fooyySiyXEF
  foo(a2)
  // CHECK: function_ref @$s12rdar371606793baryySiyKXEF
  bar(b1)
  // CHECK: function_ref @$s12rdar371606793baryySiyXEF
  bar(b2)
}
