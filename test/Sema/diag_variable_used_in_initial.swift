// RUN: %target-swift-frontend -typecheck -verify %s

class A1 {
  func foo1() {}
  func foo2() {
    var foo1 = foo1() // expected-error {{variable used within its own initial value; use 'self.' to refer to the instance method}}{{16-16=self.}}
  }
}

class A2 {
  var foo1 = 2
  func foo2() {
    // FIXME: "the var" doesn't sound right.
    var foo1 = foo1 // expected-error {{variable used within its own initial value; use 'self.' to refer to the var}}{{16-16=self.}}
  }
}

class A3 {
  func foo2() {
    // FIXME: this should also add fixit.
    var foo1 = foo1() // expected-error {{variable used within its own initial value}}{{none}}
  }
  func foo1() {}
}

class A4 {
  func foo2() {
    var foo1 = foo1 // expected-error {{variable used within its own initial value}}{{none}}
  }
}

func localContext() {
  class A5 {
    func foo1() {}
    func foo2() {
      var foo1 = foo1() // expected-error {{variable used within its own initial value; use 'self.' to refer to the instance method}}{{18-18=self.}}
    }

    class A6 {
      func foo1() {}
      func foo2() {
        var foo1 = foo1() // expected-error {{variable used within its own initial value; use 'self.' to refer to the instance method}}{{20-20=self.}}
      }
    }
  }
}
