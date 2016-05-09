// RUN: %target-parse-verify-swift

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
}

func testFunctionsInClass(c1 : C1) {
  C1.f1Static()     // okay
  C1.f2Static()     // expected-warning {{result of call to 'f2Static()' is unused}}
  _ = C1.f2Static() // okay

  C1.f1Class()      // okay
  C1.f2Class()      // expected-warning {{result of call to 'f2Class()' is unused}}
  _ = C1.f2Class()  // okay

  C1()              // expected-warning {{result of initializer is unused}}
  _ = C1()          // okay
  C1(foo: 5)        // expected-warning {{result of call to 'init(foo:)' is unused}}
  _ = C1(foo: 5)    // okay

  c1.f1()           // okay
  c1.f2()           // expected-warning {{result of call to 'f2()' is unused}}
  _ = c1.f2()       // okay
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
}

func testFunctionsInStruct(s1 : S1) {
  S1.f1Static()     // okay
  S1.f2Static()     // expected-warning {{result of call to 'f2Static()' is unused}}
  _ = S1.f2Static() // okay

  S1()              // expected-warning {{result of initializer is unused}}
  _ = S1()          // okay
  S1(foo: 5)        // expected-warning {{result of call to 'init(foo:)' is unused}}
  _ = S1(foo: 5)    // okay

  s1.f1()           // okay
  s1.f2()           // expected-warning {{result of call to 'f2()' is unused}}
  _ = s1.f2()       // okay
}
