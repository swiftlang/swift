// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking
// REQUIRES: swift_swift_parser

// Test the behavior of prefix '/' with regex literals enabled.

prefix operator /
prefix func / <T> (_ x: T) -> T { x }

enum E {
  case e
  func foo<T>(_ x: T) {}
}

_ = /E.e
(/E.e).foo(/0)

func foo<T, U>(_ x: T, _ y: U) {}
foo(/E.e, /E.e)
foo((/E.e), /E.e)
foo((/)(E.e), /E.e)

func bar<T>(_ x: T) -> Int { 0 }
_ = bar(/E.e) / 2
