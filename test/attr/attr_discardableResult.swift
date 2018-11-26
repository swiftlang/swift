// RUN: %target-typecheck-verify-swift -enable-objc-interop

// ---------------------------------------------------------------------------
// Mark function's return value as discardable and silence warning
// ---------------------------------------------------------------------------

@discardableResult
func f1() -> [Int] { }

func f2() -> [Int] { }

func f3() { }

func f4<R>(blah: () -> R) -> R { return blah() }

func testGlobalFunctions() -> [Int] {
  f1()        // okay
  f2()        // expected-warning {{result of call to 'f2()' is unused}}
  _ = f2()    // okay
  f3()        // okay
  f4 { 5 }    // expected-warning {{result of call to 'f4(blah:)' is unused}}
  f4 { }      // okay
  return f2() // okay
}

attr_discardableResult.f1()
attr_discardableResult.f2() // expected-warning {{result of call to 'f2()' is unused}}

class C1 {
  @discardableResult
  static func f1Static() -> Int { }

  static func f2Static() -> Int { }

  @discardableResult
  class func f1Class() -> Int { }

  class func f2Class() -> Int { }

  @discardableResult init() { }
  init(foo: Int) { }

  @discardableResult
  func f1() -> Int { }

  func f2() -> Int { }

  @discardableResult
  func f1Optional() -> Int? { }

  func f2Optional() -> Int? { }

  @discardableResult
  func me() -> Self { return self }

  func reallyMe() -> Self { return self }
}

class C2 : C1 {}

func testFunctionsInClass(c1 : C1, c2: C2) {
  C1.f1Static()     // okay
  C1.f2Static()     // expected-warning {{result of call to 'f2Static()' is unused}}
  _ = C1.f2Static() // okay

  C1.f1Class()      // okay
  C1.f2Class()      // expected-warning {{result of call to 'f2Class()' is unused}}
  _ = C1.f2Class()  // okay

  C1()              // okay, marked @discardableResult
  _ = C1()          // okay
  C1(foo: 5)        // expected-warning {{result of 'C1' initializer is unused}}
  _ = C1(foo: 5)    // okay

  c1.f1()           // okay
  c1.f2()           // expected-warning {{result of call to 'f2()' is unused}}
  _ = c1.f2()       // okay

  c1.f1Optional()   // okay
  c1.f2Optional()   // expected-warning {{result of call to 'f2Optional()' is unused}}
  _ = c1.f2Optional() // okay

  c1.me()           // okay
  c2.me()           // okay

  c1.reallyMe()     // expected-warning {{result of call to 'reallyMe()' is unused}}
  c2.reallyMe()     // expected-warning {{result of call to 'reallyMe()' is unused}}

  _ = c1.reallyMe() // okay
  _ = c2.reallyMe() // okay
}

struct S1 {
  @discardableResult
  static func f1Static() -> Int { }

  static func f2Static() -> Int { }

  @discardableResult init() { }
  init(foo: Int) { }

  @discardableResult
  func f1() -> Int { }

  func f2() -> Int { }

  @discardableResult
  func f1Optional() -> Int? { }

  func f2Optional() -> Int? { }
}

func testFunctionsInStruct(s1 : S1) {
  S1.f1Static()     // okay
  S1.f2Static()     // expected-warning {{result of call to 'f2Static()' is unused}}
  _ = S1.f2Static() // okay

  S1()              // okay, marked @discardableResult
  _ = S1()          // okay
  S1(foo: 5)        // expected-warning {{result of 'S1' initializer is unused}}
  _ = S1(foo: 5)    // okay

  s1.f1()           // okay
  s1.f2()           // expected-warning {{result of call to 'f2()' is unused}}
  _ = s1.f2()       // okay

  s1.f1Optional()   // okay
  s1.f2Optional()   // expected-warning {{result of call to 'f2Optional()' is unused}}
  _ = s1.f2Optional() // okay
}

protocol P1 {
  @discardableResult
  func me() -> Self

  func reallyMe() -> Self
}

func testFunctionsInExistential(p1 : P1) {
  p1.me()           // okay
  p1.reallyMe()     // expected-warning {{result of call to 'reallyMe()' is unused}}
  _ = p1.reallyMe() // okay
}

let x = 4
"Hello \(x+1) world"  // expected-warning {{string literal is unused}}

func f(a : () -> Int) {
  42  // expected-warning {{integer literal is unused}}
  
  4 + 5 // expected-warning {{result of operator '+' is unused}}
  a() // expected-warning {{result of call to function returning 'Int' is unused}}
}

@warn_unused_result func g() -> Int { } // expected-warning {{'warn_unused_result' attribute behavior is now the default}} {{1-21=}}

class X {
  @warn_unused_result // expected-warning {{'warn_unused_result' attribute behavior is now the default}} {{3-23=}}
  @objc
  func h() -> Int { }
}

func testOptionalChaining(c1: C1?, s1: S1?) {
  c1?.f1()         // okay
  c1!.f1()         // okay
  c1?.f1Optional() // okay
  c1!.f1Optional() // okay
  c1?.f2()         // expected-warning {{result of call to 'f2()' is unused}}
  c1!.f2()         // expected-warning {{result of call to 'f2()' is unused}}
  c1?.f2Optional() // expected-warning {{result of call to 'f2Optional()' is unused}}
  c1!.f2Optional() // expected-warning {{result of call to 'f2Optional()' is unused}}

  s1?.f1()         // okay
  s1!.f1()         // okay
  s1?.f1Optional() // okay
  s1!.f1Optional() // okay
  s1?.f2()         // expected-warning {{result of call to 'f2()' is unused}}
  s1!.f2()         // expected-warning {{result of call to 'f2()' is unused}}
  s1?.f2Optional() // expected-warning {{result of call to 'f2Optional()' is unused}}
  s1!.f2Optional() // expected-warning {{result of call to 'f2Optional()' is unused}}
}

@discardableResult func SR2948 (_ closure: @escaping ()->()) -> (()->()) {
  closure()
  return closure
}
SR2948({}) // okay

class SR7562_A {
    @discardableResult required init(input: Int) { }
}

class SR7562_B : SR7562_A {}

SR7562_A(input: 10) // okay
SR7562_B(input: 10) // okay
