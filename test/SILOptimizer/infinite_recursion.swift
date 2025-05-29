// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify

// REQUIRES: swift_in_compiler

func a() {
  a()  // expected-warning {{function call causes an infinite recursion}}
}

func throwing_func() throws {
  try throwing_func()  // expected-warning {{function call causes an infinite recursion}}
}

func b(_ x : Int) {
  if x != 0 {
    b(x)  // expected-warning {{function call causes an infinite recursion}}
  } else {
    b(x+1)  // expected-warning {{function call causes an infinite recursion}}
  }
}

func noInvariantArgs(_ x : Int) {
  if x != 0 {
    noInvariantArgs(x-1)  // expected-warning {{function call causes an infinite recursion}}
  } else {
    noInvariantArgs(x+1)  // expected-warning {{function call causes an infinite recursion}}
  }
}

func c(_ x : Int) {
  if x != 0 {
    c(5)
  }
}

func invariantArgCondition(_ x : Int) {
  if x != 0 {
    invariantArgCondition(x)  // expected-warning {{function call causes an infinite recursion}}
  }
}

func invariantLoopCondition(_ x : Int) {
  while x != 0 {
    invariantLoopCondition(x)  // expected-warning {{function call causes an infinite recursion}}
  }
}

final class ClassWithInt {
  var i: Int = 0
}

func invariantMemCondition(_ c: ClassWithInt) {
  if c.i > 0 {
    invariantMemCondition(c)  // expected-warning {{function call causes an infinite recursion}}
  }
}

func variantMemCondition(_ c: ClassWithInt) {
  if c.i > 0 {
    c.i -= 1
    invariantMemCondition(c)  // no warning
  }
}

func nestedInvariantCondition(_ x : Int, _ y: Int) {
  if x > 0 {
    if y != 0 {
      if x == 0 {
        nestedInvariantCondition(x, y)  // expected-warning {{function call causes an infinite recursion}}
      }
    }
  }
}

func nestedVariantCondition(_ x : Int, _ y: Int) {
  if x > 0 {
    if y != 0 {
      if x == 0 {
        nestedVariantCondition(x, y - 1) // no warning
      }
    }
  }
}

func multipleArgs1(_ x : Int, _ y : Int) {
  if y > 0 {
    multipleArgs1(x - 1, y)  // expected-warning {{function call causes an infinite recursion}}
  } else if x > 10 {
    multipleArgs1(x - 2, y)
  }
}

func multipleArgs2(_ x : Int, _ y : Int) {
  if y > 0 {
    multipleArgs2(x, y - 1)  // expected-warning {{function call causes an infinite recursion}}
  } else if x > 10 {
    multipleArgs2(x, y - 2)  // expected-warning {{function call causes an infinite recursion}}
  }
}

func multipleArgsNoWarning(_ x : Int, _ y : Int) {
  if y > 0 {
    multipleArgsNoWarning(x, y - 1)
  } else if x > 10 {
    multipleArgsNoWarning(x - 1, y)
  }
}

struct A {}
struct B {}

func deadEndBlockInElseBranch(_ x : Int) {
  if x != 0 {
    deadEndBlockInElseBranch(x - 1) // no warning
  } else {
    _ = unsafeBitCast(A(), to: B.self)
  }
}


struct Str {
  var x = 27

  mutating func writesMemory() {
    if x > 0 {
      x -= 1
      writesMemory() // no warning
    }
  }

  mutating func doesNotWriteMem() {
    if x > 0 {
      doesNotWriteMem()  // expected-warning {{function call causes an infinite recursion}}
    }
  }

  func nonMutating() {
    if x > 0 {
      nonMutating()  // expected-warning {{function call causes an infinite recursion}}
    }
  }
}

func d(_ x : Int) {
  var x = x
  if x != 0 {
    x += 1
  }
  return d(x)  // expected-warning {{function call causes an infinite recursion}}
}

// Doesn't warn on mutually recursive functions

func e() { f() }
func f() { e() }

func g() {
  while true {
    g() // expected-warning {{function call causes an infinite recursion}}
  }

  g()
}

func h(_ x : Int) {
  while (x < 5) {
    h(x+1)
  }
}

func i(_ x : Int) {
  var x = x
  while (x < 5) {
    x -= 1
  }
  i(0)  // expected-warning {{function call causes an infinite recursion}}
}

func j() -> Int {
  return 5 + j()  // expected-warning {{function call causes an infinite recursion}}
}

func k() -> Any {
  return type(of: k())  // expected-warning {{function call causes an infinite recursion}}
}

@_silgen_name("exit") func exit(_: Int32) -> Never

func l() {
  guard Bool.random() else {
    exit(0) // no warning; calling 'exit' terminates the program
  }
  l()
}

func m() {
  guard Bool.random() else {
    fatalError() // we _do_ warn here, because fatalError is a programtermination_point
  }
  m() // expected-warning {{function call causes an infinite recursion}}
}

enum MyNever {}

func blackHole() -> MyNever {
  blackHole() // expected-warning {{function call causes an infinite recursion}}
}

@_semantics("programtermination_point")
func terminateMe() -> MyNever {
  terminateMe() // no warning; terminateMe is a programtermination_point
}

func n() -> MyNever {
  if Bool.random() {
    blackHole() // no warning; blackHole() will terminate the program
  }
  n()
}

func o() -> MyNever {
  if Bool.random() {
    o()
  }
  blackHole() // no warning; blackHole() will terminate the program
}

func mayHaveSideEffects() {}

func p() {
  if Bool.random() {
    mayHaveSideEffects() // presence of side-effects doesn't alter the check for the programtermination_point apply
    fatalError()
  }
  p() // expected-warning {{function call causes an infinite recursion}}
}

class S {
  convenience init(a: Int) {
    self.init(a: a) // expected-warning {{function call causes an infinite recursion}}
  }
  init(a: Int?) {}

  static func a() {
    return a() // expected-warning {{function call causes an infinite recursion}}
  }

  func b() {
    var i = 0
    repeat {
      i += 1
      b() // expected-warning {{function call causes an infinite recursion}}
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
    set {
      self.bar = newValue // expected-warning {{function call causes an infinite recursion}}
    }
  }
}

public class U {
  required convenience init(x: Int) {
    self.init(x: x) // expected-warning {{function call causes an infinite recursion}}
  }
}

func == (l: S?, r: S?) -> Bool {
  if l == nil && r == nil { return true } // expected-warning {{function call causes an infinite recursion}}
  guard let l = l, let r = r else { return false }
  return l === r
}

public func == <Element>(lhs: Array<Element>, rhs: Array<Element>) -> Bool {
  return lhs == rhs // expected-warning {{function call causes an infinite recursion}}
}

func factorial(_ n : UInt) -> UInt {
  return (n != 0) ? factorial(n - 1) * n : factorial(1) // expected-warning {{function call causes an infinite recursion}}
                                                        // expected-warning @-1 {{function call causes an infinite recursion}}

}

func tr(_ key: String) -> String {
  return tr(key) ?? key // expected-warning {{left side of nil coalescing operator '??' has non-optional type}}
                        // expected-warning @-1 {{function call causes an infinite recursion}}
}

class Node {
  var parent: Node?
  var rootNode: RootNode {
    return parent!.rootNode // No warning - has an override.
  }
}

class RootNode: Node {
  override var rootNode: RootNode { return self }
}

protocol P {
  associatedtype E: P
}

func noRecursionMismatchingTypeArgs1<T: P>(_ t: T.Type) {
  if T.self == Int.self {
    return
  }
  noRecursionMismatchingTypeArgs1(T.E.self)
}

func noRecursionMismatchingTypeArgs2<T: P>(_ t: T.Type) {
  if MemoryLayout<T>.size == 1 {
    return
  }
  noRecursionMismatchingTypeArgs2(T.E.self)
}

func recursionMatchingTypeArgs1<T: P>(_ t: T.Type) {
  if MemoryLayout<T>.size == 1 {
    return
  }
  recursionMatchingTypeArgs1(T.self) // expected-warning {{function call causes an infinite recursion}}
}

func noRecursionMismatchingTypeArgs3<T: P, V: P>(_ t: T.Type, _ v: V.Type) {
  if MemoryLayout<T>.size == 1 {
    return
  }
  noRecursionMismatchingTypeArgs3(V.self, T.self)
}

func recursionMatchingTypeArgs2<T: P, V: P>(_ t: T.Type, _ v: V.Type) {
  if MemoryLayout<T>.size == 1 {
    return
  }
  recursionMatchingTypeArgs2(T.self, V.self) // expected-warning {{function call causes an infinite recursion}}
}

