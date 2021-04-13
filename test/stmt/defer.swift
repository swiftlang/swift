// RUN: %target-typecheck-verify-swift

func voidReturn1() {}
func breakContinue(_: Int) -> Int {}

func testDefer(_ a : Int) {
  
  defer { voidReturn1() }
  defer { breakContinue(1)+42 } // expected-warning {{result of operator '+' is unused}}
  
  // Ok:
  defer { while false { break } }

  // Not ok.
  while false { defer { break } }   // expected-error {{'break' cannot transfer control out of a defer statement}}
  // expected-warning@-1 {{'defer' statement at end of scope always executes immediately}}{{17-22=do}}
  defer { return }  // expected-error {{'return' cannot transfer control out of a defer statement}}
  // expected-warning@-1 {{'defer' statement at end of scope always executes immediately}}{{3-8=do}}
}

class SomeTestClass {
  var x = 42
 
  func method() {
    defer { x = 97 }  // self. not required here!
    // expected-warning@-1 {{'defer' statement at end of scope always executes immediately}}{{5-10=do}}
  }
}

enum DeferThrowError: Error {
  case someError
}

func throwInDefer() {
  defer { throw DeferThrowError.someError } // expected-error {{errors cannot be thrown out of a defer body}}
  print("Foo")
}

func throwInDeferOK1() {
  defer {
    do {
      throw DeferThrowError.someError
    } catch {}
  }
  print("Bar")
}

func throwInDeferOK2() throws {
  defer {
    do {
      throw DeferThrowError.someError
    } catch {}
  }
  print("Bar")
}

func throwingFuncInDefer1() throws {
  defer { try throwingFunctionCalledInDefer() } // expected-error {{errors cannot be thrown out of a defer body}}
  print("Bar")
}

func throwingFuncInDefer1a() throws {
  defer {
    do {
      try throwingFunctionCalledInDefer()
    } catch {}
  }
  print("Bar")
}

func throwingFuncInDefer2() throws {
  defer { throwingFunctionCalledInDefer() } // expected-error {{errors cannot be thrown out of a defer body}}
  print("Bar")
}

func throwingFuncInDefer2a() throws {
  defer {
    do {
      throwingFunctionCalledInDefer()
      // expected-error@-1 {{call can throw but is not marked with 'try'}}
      // expected-note@-2 {{did you mean to use 'try'?}}
      // expected-note@-3 {{did you mean to handle error as optional value?}}
      // expected-note@-4 {{did you mean to disable error propagation?}}
    } catch {}
  }
  print("Bar")
}

func throwingFuncInDefer3() {
  defer { try throwingFunctionCalledInDefer() } // expected-error {{errors cannot be thrown out of a defer body}}
  print("Bar")
}

func throwingFuncInDefer3a() {
  defer {
    do {
      try throwingFunctionCalledInDefer()
    } catch {}
  }
  print("Bar")
}

func throwingFuncInDefer4() {
  defer { throwingFunctionCalledInDefer() } // expected-error {{errors cannot be thrown out of a defer body}}
  print("Bar")
}

func throwingFuncInDefer4a() {
  defer {
    do {
      throwingFunctionCalledInDefer()
      // expected-error@-1 {{call can throw but is not marked with 'try'}}
      // expected-note@-2 {{did you mean to use 'try'?}}
      // expected-note@-3 {{did you mean to handle error as optional value?}}
      // expected-note@-4 {{did you mean to disable error propagation?}}
    } catch {}
  }
  print("Bar")
}

func throwingFunctionCalledInDefer() throws {
  throw DeferThrowError.someError
}

class SomeDerivedClass: SomeTestClass {
  override init() {
    defer {
      super.init() // expected-error {{initializer chaining ('super.init') cannot be nested in another expression}}
    }
  }
}

// rdar://75088379 -- 'defer' should not be able to forward-reference captures
func badForwardReference() {
  defer {
    _ = x2 // expected-error {{use of local variable 'x2' before its declaration}}

    let x1 = 0
    let y1 = 0

    defer {
      _ = x1
      _ = x2 // expected-error {{use of local variable 'x2' before its declaration}}
      _ = y1
      _ = y2 // expected-error {{use of local variable 'y2' before its declaration}}
    }

    let y2 = 0 // expected-note {{'y2' declared here}}
  }

  let x2 = 0 // expected-note 2{{'x2' declared here}}

  func localFunc() {
    defer {
      _ = z1 // expected-error {{use of local variable 'z1' before its declaration}}
      _ = z2
    }

    let z1 = 0 // expected-note {{'z1' declared here}}
  }

  let z2 = 0
}
