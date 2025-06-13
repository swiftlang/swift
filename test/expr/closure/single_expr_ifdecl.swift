// RUN: %target-typecheck-verify-swift

func takeIntToInt(_ f: (Int) -> Int) { }
func takeIntIntToInt(_ f: (Int, Int) -> Int) { }

// Simple closures with anonymous arguments
func simple() {
  takeIntToInt({
    #if true
    $0 + 1
    #else 
    $0 + 2
    #endif
  })
  takeIntIntToInt({
    #if true
    $0 + $1 + 1
    #else 
    $0 + $1 + 2
    #endif
  })
}

// Anonymous arguments with inference
func myMap<T, U>(_ array: [T], _ f: (T) -> U) -> [U] {}

func testMap(_ array: [Int]) {
  var farray = myMap(array, {
    #if true
    Float($0)
    #else
    Float($0 + 1)
    #endif
  })
  var _ : Float = farray[0]
  let farray2 = myMap(array, { (x : Int) in
    #if true
    Float(x)
    #else
    Float(x + 1)
    #endif
  })
  farray = farray2
  _ = farray
}

// Nested single-expression closures -- <rdar://problem/20931915>
class NestedSingleExpr {
  private var b: Bool = false
  private func callClosureA(_ callback: () -> Void) {}
  private func callClosureB(_ callback: () -> Void) {}

  func call() {
    callClosureA { [weak self] in
      #if true
      self?.callClosureA {
        #if true
        self?.b = true
        #else
        self?.b = false
        #endif
      }
      #else
      self?.callClosureB {
        #if true
        self?.b = true
        #else
        self?.b = false
        #endif
      }
      #endif
    }
  }
}

// Autoclosure nested inside single-expr closure should get discriminator
// <rdar://problem/22441425> Swift compiler "INTERNAL ERROR: this diagnostic should not be produced"
struct Expectation<T> {}
func expect<T>(_ expression: @autoclosure () -> T) -> Expectation<T> {
  return Expectation<T>()
}
func describe(_ closure: () -> ()) {}
func f() {
  #if true
  describe {
    #if false
    _ = expect("this")
    #else
    _ = expect("what")
    #endif
  }
  #endif
}

struct Blob {}

func withBlob(block: (Blob) -> ()) {}

protocol Binding {}
extension Int: Binding {}
extension Double: Binding {}
extension String: Binding {}
extension Blob: Binding {}

struct Stmt {
  @discardableResult
  func bind(_ values: Binding?...) -> Stmt {
    return self
  }

  @discardableResult
  func bind(_ values: [Binding?]) -> Stmt {
    return self
  }

  @discardableResult
  func bind(_ values: [String: Binding?]) -> Stmt {
    return self
  }
}

let stmt = Stmt()
withBlob {
  #if true
  stmt.bind(1, 2.0, "3", $0)
  #endif
}
withBlob {
  #if true
  stmt.bind([1, 2.0, "3", $0])
  #endif
}
withBlob {
  #if true
  stmt.bind(["1": 1, "2": 2.0, "3": "3", "4": $0])
  #endif
}

// <rdar://problem/19840785>
// We shouldn't crash on the call to 'a.dispatch' below.
class A {
  func dispatch(_ f : () -> Void) {
    f()
  }
}

class C {
	var prop = 0
	var a = A()

	func act() {
    a.dispatch({() -> Void in
      #if true
      self.prop // expected-warning {{property is accessed but result is unused}}
      #endif
    })
  }
}

// Never-returning expressions
func haltAndCatchFire() -> Never {
  #if true
  while true { }
  #else
  while false { }
  #endif
}
let backupPlan: () -> Int = {
  #if true
  haltAndCatchFire()
  #endif
}
func missionCritical(storage: () -> String) {}
missionCritical(storage: {
  #if true
  haltAndCatchFire()
  #endif
})

// https://github.com/apple/swift/issues/47540
enum E { }
func takesAnotherUninhabitedType(e: () -> E) {}
takesAnotherUninhabitedType {
  #if true
  haltAndCatchFire()
  #endif
}

// Weak capture bug caught by rdar://problem/67351438
class Y {
  var toggle: Bool = false

  func doSomething(animated: Bool, completionHandler: (Int, Int) -> Void) { }
}

class X {
  private(set) var someY: Y!

  func doSomething() {
    someY?.doSomething(animated: true, completionHandler: { [weak someY] _, _ in
      #if true
      someY?.toggle = true
      #else
      someY?.toggle = false
      #endif
    })
  }
}

var intOrStringClosure_true = {
  #if true
  42
  #else
  "foo"
  #endif
}

var intOrStringClosure_false = {
  #if false
  42
  #else
  "foo"
  #endif
}

func testMultiType() {
  
  let a = intOrStringClosure_true()
  _ = a as Int
  _ = a as String // expected-error {{cannot convert value of type 'Int' to type 'String'}}
  
  let b = intOrStringClosure_false()
  _ = b as Int // expected-error {{cannot convert value of type 'String' to type 'Int'}}
  _ = b as String
}
