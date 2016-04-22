// RUN: %target-parse-verify-swift

// Test "near misses" where a member of a class or extension thereof
// nearly matches an optional requirement, but does not exactly match.

@objc protocol P1 {
  optional func doSomething(a: Int, b: Double) // expected-note 2{{requirement 'doSomething(a:b:)' declared here}}
}

class C1a : P1 {
  @objc func doSomething(a: Int, c: Double) { }
  // expected-warning@-1{{instance method 'doSomething(a:c:)' nearly matches optional requirement 'doSomething(a:b:)' of protocol 'P1'}}
  // expected-note@-2{{rename to 'doSomething(a:b:)' to satisfy this requirement}}{{34-34=b }}{{8-8=(doSomethingWithA:b:)}}
  // expected-note@-3{{move 'doSomething(a:c:)' to an extension to silence this warning}}
  // expected-note@-4{{make 'doSomething(a:c:)' private to silence this warning}}{{9-9=private }}
}

class C1b : P1 {
}

extension C1b {
  @objc func doSomething(a: Int, c: Double) { } // don't warn
}

class C1c {
}

extension C1c : P1 {
  func doSomething(a: Int, c: Double) { }
  // expected-warning@-1{{instance method 'doSomething(a:c:)' nearly matches optional requirement 'doSomething(a:b:)' of protocol 'P1'}}
  // expected-note@-2{{rename to 'doSomething(a:b:)' to satisfy this requirement}}{{28-28=b }}{{3-3=@objc(doSomethingWithA:b:) }}
  // expected-note@-3{{move 'doSomething(a:c:)' to another extension to silence this warning}}
  // expected-note@-4{{make 'doSomething(a:c:)' private to silence this warning}}{{3-3=private }}
}

class C1d : P1 {
  @objc private func doSomething(a: Int, c: Double) { } // don't warn
}

class C1e : P1 {
  @nonobjc func doSomething(a: Int, c: Double) { } // don't warn
}

// Don't try to match an optional requirement against a declaration
// that already satisfies one witness.
@objc protocol P2 {
  optional func doSomething(a: Int, b: Double)
  optional func doSomething(a: Int, d: Double)
}

class C2a : P2 {
  @objc func doSomething(a: Int, b: Double) { } // don't warn: this matches something
}

// Cope with base names that change.
@objc protocol P3 {
  optional func doSomethingWithPriority(_ a: Int, d: Double) // expected-note{{requirement 'doSomethingWithPriority(_:d:)' declared here}}
}

class C3a : P3 {
  func doSomething(priority: Int, d: Double) { }
  // expected-warning@-1{{instance method 'doSomething(priority:d:)' nearly matches optional requirement 'doSomethingWithPriority(_:d:)' of protocol 'P3'}}
  // expected-note@-2{{rename to 'doSomethingWithPriority(_:d:)' to satisfy this requirement}}{{20-20=_ }}
  // expected-note@-3{{move 'doSomething(priority:d:)' to an extension to silence this warning}}
  // expected-note@-4{{make 'doSomething(priority:d:)' private to silence this warning}}{{3-3=private }}
}

@objc protocol P4 {
  optional func doSomething(priority: Int, d: Double) // expected-note{{requirement 'doSomething(priority:d:)' declared here}}
}

class C4a : P4 {
  func doSomethingWithPriority(_ a: Int, d: Double) { }
  // expected-warning@-1{{instance method 'doSomethingWithPriority(_:d:)' nearly matches optional requirement 'doSomething(priority:d:)' of protocol 'P4'}}
  // expected-note@-2{{rename to 'doSomething(priority:d:)' to satisfy this requirement}}{{32-33=priority}}
  // expected-note@-3{{move 'doSomethingWithPriority(_:d:)' to an extension to silence this warning}}
  // expected-note@-4{{make 'doSomethingWithPriority(_:d:)' private to silence this warning}}{{3-3=private }}
}
