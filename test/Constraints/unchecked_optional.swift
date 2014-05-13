// RUN: %swift -parse -verify %s

class A {
  func do_a() {}

  func do_b(x: Int) {}
  func do_b(x: Float) {}

  func do_c(#x: Int) {} // expected-note 2 {{found this candidate}}
  func do_c(#y: Int) {} // expected-note 2 {{found this candidate}}
}

func test0(a : A!) {
  a.do_a()

  a.do_b(1)
  a.do_b(5.0)

  a.do_c(1) // expected-error {{ambiguous use of 'do_c'}}
  a.do_c(x: 1)
}

func test1(a : A!) {
  a?.do_a()

  a?.do_b(1)
  a?.do_b(5.0)

  a?.do_c(1) // expected-error {{ambiguous use of 'do_c'}}
  a?.do_c(x: 1)
}

struct B {
  var x : Int
}

func test2(var b : B!) {
  var x = b.x
  b.x = x // expected-error {{cannot assign to 'x' in 'b'}}
}

struct Subscriptable {
  subscript(x : Int) -> Int {
    get {
      return x
    }
  }
}

func test3(x: Subscriptable!) -> Int {
  return x[0]
}

// Callable
func test4(f: (Int -> Float)!) -> Float {
  return f(5)
}

func test5(value : Int!) {
  let value2 : Int? = value
}

func test6(value : Int!) {
  let value2 : Int? = value
}

struct Test7a {}
struct Test7b {
  @conversion func __conversion() -> Test7a! { return .None }
}
var test7var: Test7a? = Test7b()

struct Test8 {
  @conversion func __conversion<T>() -> T! { return .None }
}
protocol Test8p {}
var test8var: Test8p? {
  return Test8()
}

class Test9a {}
class Test9b : Test9a {}
func test9_produceUnchecked() -> Test9b! { return Test9b() }
func test9_consume(foo : Test9b) {}
func test9() -> Test9a {
  var foo = test9_produceUnchecked()
  test9_consume(foo)
  var bar : Test9a = foo
  return foo
}

func test10_helper(x : Int!) -> Int? { return x }
func test10(x : Int?) -> Int! { return test10_helper(x) }
