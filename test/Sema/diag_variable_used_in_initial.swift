// RUN: %target-typecheck-verify-swift

class A1 {
  func foo1() -> Int {}
  func foo2() {
    var foo1 = foo1()
    // expected-warning@-1 {{initialization of variable 'foo1' was never used; consider replacing with assignment to '_' or removing it}}
  }
}

class A2 {
  var foo1 = 2
  func foo2() {
    var foo1 = foo1
    // expected-warning@-1 {{initialization of variable 'foo1' was never used; consider replacing with assignment to '_' or removing it}}
  }
}

class A3 {
  func foo2() {
    var foo1 = foo1()
    // expected-warning@-1 {{initialization of variable 'foo1' was never used; consider replacing with assignment to '_' or removing it}}
  }
  func foo1() -> Int {}
}

class A4 {
  func foo2() {
    var foo1 = foo1 // expected-error {{use of local variable 'foo1' before its declaration}}
    // expected-note@-1 {{'foo1' declared here}}
  }
}

func localContext() {
  class A5 {
    func foo1() -> Int {}
    func foo2() {
      var foo1 = foo1()
      // expected-warning@-1 {{initialization of variable 'foo1' was never used; consider replacing with assignment to '_' or removing it}}
    }

    class A6 {
      func foo1() -> Int {}
      func foo2() {
        var foo1 = foo1()
        // expected-warning@-1 {{initialization of variable 'foo1' was never used; consider replacing with assignment to '_' or removing it}}
      }
    }

    extension E { // expected-error {{declaration is only valid at file scope}}
      // expected-error@-1{{cannot find type 'E' in scope}}
      class A7 {
        func foo1() {}
        func foo2() {
          var foo1 = foo1()
        }
      }
    }
  }
}

// https://github.com/swiftlang/swift/issues/84909

func uninit_closure_reference() {
  func passthrough(_ a: () -> Any) -> Any { a() }

  let initMe = passthrough { initMe }
  // expected-error @-1 {{use of local variable 'initMe' before its declaration}}
  // expected-note @-2 {{default value declared here}}

  let inline = // expected-note {{default value declared here}}
  {
    () -> Any in
    inline // expected-error {{use of local variable 'inline' before its declaration}}
  }()

  // these should not regress
  func castAny(_ a: Any) {
    let directUncond = a as! Int
    _ = directUncond

    let directCond = a as? Int
    _ = directCond

    let twoPhaseUncond: Int
    twoPhaseUncond = a as! Int
    _ = twoPhaseUncond

    let twoPhaseCond: Int?
    twoPhaseCond = a as? Int
    _ = twoPhaseCond
  }
}
