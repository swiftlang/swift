// RUN: %target-parse-verify-swift -parse-as-library

struct S {
  func foo() {
    super.foo() // expected-error{{'super' members cannot be referenced in a non-class type}}
  }
}

class D : B {
  func b_foo() -> Int { return super.foo }

  override func bar(a: Float) -> Int { return super.bar(a) }

  func bas() -> (Int, UnicodeScalar, String) {
    return (super.zim(), super.zang(), super.zung())
  }

  override var zippity : Int { return super.zippity }
}

extension D {
  func d_ext_foo() -> Int {
    return super.foo
  }
}

class B {
  var foo : Int = 0
  func bar(a: Float) -> Int {}

  func zim() -> Int {}
  func zang() -> UnicodeScalar {}
  func zung() -> String {}

  var zippity : Int { return 123 }

  func zoo() { super.zoo() } // expected-error{{'super' members cannot be referenced in a root class}}

}

class X<T> {
  func method<U>(x: T, y: U) { }
}

class Y<U> : X<Int> {
  func otherMethod<U>(x: Int, y: U) {
    super.method(x, y: y)
  }
}

func use_d(d: D) -> Int {
  d.b_foo()
  d.bar(1.0)
  d.bas()

  return d.zippity
}

func not_method() {
  super.foo() // expected-error{{'super' members cannot be referenced in a non-class type}}
}
