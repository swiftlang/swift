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

struct SR7249<T> {
    var x: T { fatalError() } // expected-note {{previously declared}}
    var y: Int // expected-note {{previously declared}}
    var z: Int // expected-note {{previously declared}}
}

extension SR7249 {
    var x: Int { fatalError() } // expected-error{{invalid redeclaration of 'x'}}
    var y: T { fatalError() } // expected-error{{invalid redeclaration of 'y'}}
    var z: Int { fatalError() } // expected-error{{invalid redeclaration of 'z'}}
}
