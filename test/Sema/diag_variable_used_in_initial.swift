// RUN: %target-swift-frontend -parse -verify %s

class A1 {
  func foo1() {}
  func foo2() {
    var foo1 = foo1() // expected-error {{variable used within its own initial value}}{{16-16=self.}}
  }
}

class A2 {
  var foo1 = 2
  func foo2() {
    var foo1 = foo1 // expected-error {{variable used within its own initial value}}{{16-16=self.}}
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