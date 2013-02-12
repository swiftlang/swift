// RUN: %swift %s -constraint-checker -parse-as-library -verify

struct S {
  func foo() {
    super.foo() // expected-error{{'super' members cannot be referenced in a non-class type}}
  }
}

class D : B {
  func b_foo() -> Int { return super.foo }

  func bar(a:Float) -> Int { return super.bar(a) }

  func bas() -> (Int, Char, String) {
    return (super.zim(), super.zang(), super.zung())
  }

  var zippity : Int { return super.zippity }
}

class B {
  var foo : Int
  func bar(a:Float) -> Int {}

  func zim() -> Int {}
  func zang() -> Char {}
  func zung() -> String {}

  var zippity : Int { return 123 }

  func zoo() { super.zoo() } // expected-error{{'super' members cannot be referenced in a root class}}

}

func use_d(d:D) -> Int {
  d.b_foo()
  d.bar(1.0)
  d.bas()

  return d.zippity
}

func not_method() {
  super.foo() // expected-error{{'super' members cannot be referenced in a non-class type}}
}
