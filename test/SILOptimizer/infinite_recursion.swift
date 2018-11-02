// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify

func a() {  // expected-warning {{all paths through this function will call itself}}
  a()
}

func b(_ x : Int) {  // expected-warning {{all paths through this function will call itself}}
  if x != 0 {
    b(x)
  } else {
    b(x+1)
  }
}

func c(_ x : Int) {
  if x != 0 {
    c(5)
  }
}

func d(_ x : Int) {  // expected-warning {{all paths through this function will call itself}}
  var x = x
  if x != 0 {
    x += 1
  }
  return d(x)
}

// Doesn't warn on mutually recursive functions

func e() { f() }
func f() { e() }

func g() { // expected-warning {{all paths through this function will call itself}}
  while true { // expected-note {{condition always evaluates to true}}
    g() 
  }

  g() // expected-warning {{will never be executed}}
}

func h(_ x : Int) {
  while (x < 5) {
    h(x+1)
  }
}

func i(_ x : Int) {  // expected-warning {{all paths through this function will call itself}}
  var x = x
  while (x < 5) {
    x -= 1
  }
  i(0)
}

func j() -> Int {  // expected-warning {{all paths through this function will call itself}}
  return 5 + j()
}

func k() -> Any {  // expected-warning {{all paths through this function will call itself}}
  return type(of: k())
}

class S {
  convenience init(a: Int) { // expected-warning {{all paths through this function will call itself}}
    self.init(a: a)
  }
  init(a: Int?) {}

  static func a() { // expected-warning {{all paths through this function will call itself}}
    return a()
  }

  func b() { // expected-warning {{all paths through this function will call itself}}
    var i = 0
    repeat {
      i += 1
      b()
    } while (i > 5)
  }

  var bar: String = "hi!"
}

class T: S {
  // No warning, calls super
  override func b() {
    var i = 0
    repeat {
      i += 1
      super.b()
    } while (i > 5)
  }

  override var bar: String {
    get {
      return super.bar
    }
    set { // expected-warning {{all paths through this function will call itself}}
      self.bar = newValue
    }
  }
}

func == (l: S?, r: S?) -> Bool { // expected-warning {{all paths through this function will call itself}}
  if l == nil && r == nil { return true }
  guard let l = l, let r = r else { return false }
  return l === r
}

public func == <Element>(lhs: Array<Element>, rhs: Array<Element>) -> Bool { // expected-warning {{all paths through this function will call itself}}
  return lhs == rhs
}

func factorial(_ n : UInt) -> UInt { // expected-warning {{all paths through this function will call itself}}
  return (n != 0) ? factorial(n - 1) * n : factorial(1)
}

func tr(_ key: String) -> String { // expected-warning {{all paths through this function will call itself}}
  return tr(key) ?? key // expected-warning {{left side of nil coalescing operator '??' has non-optional type}}
}
