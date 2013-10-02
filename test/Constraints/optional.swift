// RUN: %swift -parse -verify %s

class A {
  func [objc] do_a() {}

  func [objc] do_b(x : Int) {}
  func [objc] do_b(x : Float) {}

  func [objc] do_c(x : Int) {}
  func [objc] do_c(y : Int) {}
}

func foo(a : DynamicLookup) {
  a.do_a?()

  a.do_b?(1)
  a.do_b?(5.0)

  a.do_c?(1) // expected-error {{expression does not type-check}}
  a.do_c?(x : 1)
}
