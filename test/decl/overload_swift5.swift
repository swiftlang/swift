// RUN: %target-typecheck-verify-swift -swift-version 5

// https://github.com/apple/swift/issues/49799

struct S1<T> {
  struct j {} // expected-note {{previously declared here}}
  static var k: Int { get {} } // expected-note {{previously declared here}}
}
extension S1 {
  static var i: Int { get {} } // expected-note {{previously declared here}}
  struct i {} // expected-error{{invalid redeclaration of 'i'}}
  static var j: Int { get {} } // expected-error{{invalid redeclaration of 'j'}}
  struct k {} // expected-error{{invalid redeclaration of 'k'}}
}

// https://github.com/apple/swift/issues/49797

struct S2<T> {
  var x: T { get {} } // expected-note {{previously declared}}
  var y: Int // expected-note {{previously declared}}
  var z: Int // expected-note {{previously declared}}
}
extension S2 {
  var x: Int { get {} } // expected-error{{invalid redeclaration of 'x'}}
  var y: T { get {} } // expected-error{{invalid redeclaration of 'y'}}
  var z: Int { get {} } // expected-error{{invalid redeclaration of 'z'}}
}
