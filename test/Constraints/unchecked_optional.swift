// RUN: %swift -parse -verify %s

class A {
  func do_a() {}

  func do_b(x: Int) {}
  func do_b(x: Float) {}

  func do_c(x: Int) {} // expected-note {{found this candidate}}
  func do_c(y: Int) {} // expected-note {{found this candidate}}
}

func test0(a : @unchecked A?) {
  a.do_a()

  a.do_b(1)
  a.do_b(5.0)

  a.do_c(1) // expected-error {{ambiguous use of 'do_c'}}
  a.do_c(x: 1)
}

func test1(a : @unchecked A?) {
  a?.do_a()

  a?.do_b(1)
  a?.do_b(5.0)

  // FIXME: this should really get diagnosed like the above
  a?.do_c(1) // expected-error {{expression does not type-check}}
  a?.do_c(x: 1)
}

struct B {
  var x : Int
}

func test2(var b : @unchecked B?) {
  var x = b.x
  b.x = x // expected-error {{cannot assign to the result of this expression}}
}

struct Subscriptable {
  subscript(x : Int) -> Int {
  get: return x
  }
}

func test3(x: @unchecked Subscriptable?) -> Int {
  return x[0]
}
