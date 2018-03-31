// RUN: %target-typecheck-verify-swift -swift-version 4

struct SR7251<T> {
  struct j {} // expected-note {{previously declared here}}
  static var k: Int { return 0 } // expected-note {{previously declared here}}
}
extension SR7251 {
  static var i: Int { return 0 }
  // expected-note@-1 {{previously declared here}}
  // expected-note@-2 {{previously declared here}}

  struct i {} // expected-warning {{redeclaration of 'i' is deprecated and will be illegal in Swift 5}}
  typealias i = Int // expected-warning {{redeclaration of 'i' is deprecated and will be illegal in Swift 5}}

  static var j: Int { return 0 } // expected-warning {{redeclaration of 'j' is deprecated and will be illegal in Swift 5}}

  struct k {} // expected-error{{invalid redeclaration of 'k'}}
}

