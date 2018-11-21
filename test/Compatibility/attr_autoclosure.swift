// RUN: %target-swift-frontend -swift-version 4 -emit-sil -verify %s | %FileCheck %s

do {
  func a(_ x: @autoclosure () -> Int) {}
  func b(_ x: Int, _ f: @autoclosure () -> Int = #line) {}

  func c(_ y: @autoclosure () -> Int, _ z: Int) {
    // CHECK: function_ref @$s16attr_autoclosure1aL_yySiyXKF
    a(y)

    // CHECK: function_ref @$s16attr_autoclosure1cL_yySiyXK_SitFSiyXEfu_
    b(z + 42) // ok
    b(z, y) // ok to pass `@autoclosure` function type as an argument
    b(z, y()) // ok
  }
}

func foo(_ f: @autoclosure () -> Int) {}
func foo(_ f: () -> Int) {}

do {
  func bar(_ a: @autoclosure () -> Int,
           _ b: () -> Int,
           _ c: Int) {
    // CHECK: function_ref @$s16attr_autoclosure3fooyySiyXEF
    foo(a)
    // CHECK: function_ref @$s16attr_autoclosure3fooyySiyXEF
    foo(b)

    // CHECK: function_ref @$s16attr_autoclosure3fooyySiyXKF
    foo(a())
    // CHECK: function_ref @$s16attr_autoclosure3fooyySiyXKF
    foo(b())

    // CHECK: function_ref @$s16attr_autoclosure3fooyySiyXKF
    foo(c)
  }
}

func passAutoClosureToSubscript(_ fn: @autoclosure () -> Int) {
  struct S {
    subscript(_ fn: @autoclosure () -> Int) -> Int { return fn() }
  }

  let s = S()
  let _ = s[fn] // Ok
}
