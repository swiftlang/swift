// RUN: %target-typecheck-verify-swift

struct G<T> {
  init() {}
  init<U>(x:G<U>) { }

  func foo<U>(_ x: G<U>) { }

  func bar<U>(_ x: U) { }

  static func static_foo<U>(_ x: G<U>) { }
  static func static_bar<U>(_ x: U) { }
}

typealias GInt = G<Int>
typealias GChar = G<UnicodeScalar>
GInt(x: GChar()) // expected-warning{{unused}}

GInt().foo(GChar())
GInt().bar(0)

GInt.static_foo(GChar())
GInt.static_bar(0)
