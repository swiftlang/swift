// RUN: %swift -parse -verify -constraint-checker %s

struct G<T> {
  constructor<U>(x:G<U>) { }

  func foo<U>(x:G<U>) { }

  func bar<U>(x:U) { }

  static func static_foo<U>(x:G<U>) { }
  static func static_bar<U>(x:U) { }
}

typealias GInt = G<Int>
typealias GChar = G<Char>
GInt(GChar())

GInt().foo(GChar())
GInt().bar(0)

GInt.static_foo(GChar())
GInt.static_bar(0)
