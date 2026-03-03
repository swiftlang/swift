// RUN: %target-swift-frontend -typecheck -verify %s

// Test that we emit fix-its to insert requirement stubs for the missing protocol conformance, in addition to adding the conformance.

protocol P {
  func method()
  var property: Int { get }
}

class C {
  var p: P?

  func assign() {
    p = self
    // expected-error@-1 {{cannot assign value of type 'C' to type '(any P)?'}}
    // expected-note@-2 {{add missing conformance to 'P' to class 'C'}} {{-4:8-8=: P}} {{-4:10-10=\n    func method() {\n        <#code#>\n    \}\n\n    let property: Int\n}}
  }
}

// Test that we don't emit fix-it to insert a requirement stub if there is already a satisfying witness.

class C1 {
  var p: P?

  func assign() {
    p = self
    // expected-error@-1 {{cannot assign value of type 'C1' to type '(any P)?'}}
    // expected-note@-2 {{add missing conformance to 'P' to class 'C1'}} {{-4:9-9=: P}} {{-4:11-11=\n    let property: Int\n}}
  }

  func method() {}
}

class C2 {
  var p: P?

  func assign() {
    p = self
    // expected-error@-1 {{cannot assign value of type 'C2' to type '(any P)?'}}
    // expected-note@-2 {{add missing conformance to 'P' to class 'C2'}} {{-4:9-9=: P}}
  }

  func method() {}
  var property: Int = 0
}
