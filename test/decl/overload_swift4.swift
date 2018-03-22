// RUN: %target-typecheck-verify-swift -swift-version 4

struct SR7251<T> {
  struct j {}
  static var k: Int { return 0 } // expected-note {{previously declared here}}
}
extension SR7251 {
  static var i: Int { return 0 }
  struct i {}
  static var j: Int { return 0 }
  struct k {} // expected-error{{invalid redeclaration of 'k'}}
}

