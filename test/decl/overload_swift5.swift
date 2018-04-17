// RUN: %target-typecheck-verify-swift -swift-version 5

struct SR7251<T> {
  struct j {} // expected-note {{previously declared here}}
  static var k: Int { return 0 } // expected-note {{previously declared here}}
}
extension SR7251 {
  static var i: Int { return 0 } // expected-note {{previously declared here}}
  struct i {} // expected-error{{invalid redeclaration of 'i'}}
  static var j: Int { return 0 } // expected-error{{invalid redeclaration of 'j'}}
  struct k {} // expected-error{{invalid redeclaration of 'k'}}
}

