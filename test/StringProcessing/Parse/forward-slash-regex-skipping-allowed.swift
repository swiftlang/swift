// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -parse -enable-bare-slash-regex -disable-availability-checking -experimental-skip-all-function-bodies -verify -stats-output-dir %t %s
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/stats.csv %t
// RUN: %FileCheck -input-file %t/stats.csv %s

// REQUIRES: swift_swift_parser

// Make sure we can skip in all of the below cases.

// The printing implementation differs in asserts and no-asserts builds, it will
// either print `"Parse.NumFunctionsParsed" 0` or not print it at all. Make sure
// we don't output any non-zero value.
// CHECK-NOT: {{"Parse.NumFunctionsParsed" [^0]}}

// Ensures there is a parse error
var : Int // expected-error{{expected pattern}}

// Balanced `{}`, so okay.
func a() { / {}/ }
func b() { / \{}/ }
func c() { / {"{"}/ }

// Some cases of infix '/' that we should continue to skip.
func d() {
  _ = 1 / 2 + 3 * 4
  _ = 1 / 2 / 3 / 4
}
func e() {
  let arr = [1, 2, 3]
  _ = arr.reduce(0, /) / 2

  func foo(_ i: Int, _ fn: () -> Void) {}
  foo(1 / 2 / 3, { print("}}}{{{") })
}

// Some cases of prefix '/' that we should continue to skip.
prefix operator /
prefix func / <T> (_ x: T) -> T { x }

enum E {
  case e
  func foo<T>(_ x: T) {}
}

func f() {
  _ = /E.e
  (/E.e).foo(/0)

  func foo<T, U>(_ x: T, _ y: U) {}
  foo(/E.e, /E.e)
  foo((/E.e), /E.e)
  foo((/)(E.e), /E.e)

  func bar<T>(_ x: T) -> Int { 0 }
  _ = bar(/E.e) / 2
}

postfix operator /
prefix func / <T> (_ x: T) -> T { x }

// Some cases of postfix '/' that we should continue to skip.
func g() {
    _ = 0/
    _ = 0/ / 1/
    _ = 1/ + 1/
    _ = 1 + 2/
}
