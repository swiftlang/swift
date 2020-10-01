// RUN: %target-typecheck-verify-swift

class A {
  func do_a() {}

  func do_b(_ x: Int) {}
  func do_b(_ x: Float) {}

  func do_c(x: Int) {} // expected-note 2 {{incorrect labels for candidate (have: '(_:)', expected: '(x:)')}}
  func do_c(y: Int) {} // expected-note 2 {{incorrect labels for candidate (have: '(_:)', expected: '(y:)')}}
}

func test0(_ a : A!) {
  a.do_a()

  a.do_b(1)
  a.do_b(5.0)

  a.do_c(1) // expected-error {{no exact matches in call to instance method 'do_c'}}
  a.do_c(x: 1)
}

func test1(_ a : A!) {
  a?.do_a()

  a?.do_b(1)
  a?.do_b(5.0)

  a?.do_c(1) // expected-error {{no exact matches in call to instance method 'do_c'}}
  a?.do_c(x: 1)
}

struct B {
  var x : Int
}

func test2(_ b : B!) {
  var b: B! = b
  let x = b.x
  b.x = x
  b = nil
}

struct Subscriptable {
  subscript(x : Int) -> Int {
    get {
      return x
    }
  }
}

func test3(_ x: Subscriptable!) -> Int {
  return x[0]
}

// Callable
func test4(_ f: ((Int) -> Float)!) -> Float {
  return f(5)
}

func test5(_ value : Int!) {
  let _ : Int? = value
}

func test6(_ value : Int!) {
  let _ : Int? = value
}

class Test9a {}
class Test9b : Test9a {}
func test9_produceUnchecked() -> Test9b! { return Test9b() }
func test9_consume(_ foo : Test9b) {}
func test9() -> Test9a {
  let foo = test9_produceUnchecked()!
  test9_consume(foo)
  let _ : Test9a = foo
  return foo
}

func test10_helper(_ x : Int!) -> Int? { return x }
func test10(_ x : Int?) -> Int! { return test10_helper(x) }

// Fall back to object type behind an implicitly-unwrapped optional.
protocol P11 { }
extension Int : P11 { }
func test11_helper<T : P11>(_ t: T) { }
func test11(_ i: Int!, j: Int!) {
  var j = j
  test11_helper(i)
  test11_helper(j!)
  j = nil
}
