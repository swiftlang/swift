// RUN: %target-typecheck-verify-swift -swift-version 4

// https://github.com/apple/swift/issues/49799

struct S1<T> {
  struct j {} // expected-note {{previously declared here}}
  static var k: Int { get {} } // expected-note {{previously declared here}}
}
extension S1 {
  static var i: Int { get {} }
  // expected-note@-1 {{previously declared here}}
  // expected-note@-2 {{previously declared here}}

  struct i {} // expected-warning {{redeclaration of 'i' is deprecated and will be an error in Swift 5}}
  typealias i = Int // expected-warning {{redeclaration of 'i' is deprecated and will be an error in Swift 5}}

  static var j: Int { get {} } // expected-warning {{redeclaration of 'j' is deprecated and will be an error in Swift 5}}

  struct k {} // expected-error{{invalid redeclaration of 'k'}}
}

