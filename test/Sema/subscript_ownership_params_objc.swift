// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

import Foundation

// An `inout` subscript index cannot be represented in Objective-C, exactly as
// an `inout` method parameter cannot.

class Explicit: NSObject {
  @objc subscript(i: inout Int) -> Int {
    // expected-error@-1 {{subscript cannot be marked '@objc' because inout parameters cannot be represented in Objective-C}}
    get { return i }
    set { }
  }

  @objc func method(_ x: inout Int) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because inout parameters cannot be represented in Objective-C}}

  // `borrowing` does not change the Objective-C convention, so it is fine.
  @objc subscript(b i: borrowing Int) -> Int { return i }
  @objc func borrowingMethod(_ x: borrowing Int) {}
}

@objc protocol Requirement {
  subscript(i: inout Int) -> Int { get }
  // expected-error@-1 {{subscript cannot be a member of an '@objc' protocol because inout parameters cannot be represented in Objective-C}}
  // expected-note@-2 {{inferring '@objc' because the declaration is a member of an '@objc' protocol}}
  func method(_ x: inout Int)
  // expected-error@-1 {{instance method cannot be a member of an '@objc' protocol because inout parameters cannot be represented in Objective-C}}
  // expected-note@-2 {{inferring '@objc' because the declaration is a member of an '@objc' protocol}}

  subscript(b i: borrowing Int) -> Int { get }
}

// Where '@objc' is only implicit, such a member is simply not '@objc' -- no
// diagnostic, again matching methods.
@objcMembers class Implicit: NSObject {
  subscript(i: inout Int) -> Int {
    get { return i }
    set { }
  }
  func method(_ x: inout Int) {}
}

class Unmarked: NSObject {
  subscript(i: inout Int) -> Int {
    get { return i }
    set { }
  }
}
