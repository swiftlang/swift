// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/48727
do {
  enum Key: Int {
    case aKey
    case anotherKey // expected-note {{'anotherKey' declared here}}
  }

  class C {
    var dict: [Key: String] = [:]
    func what() -> Void {
      dict[.notAKey] = "something" // expected-error {{type 'Key' has no member 'notAKey'; did you mean 'anotherKey'?}}
    }

    subscript(i: Int) -> Int { get {} }

    subscript(j: Double) -> Double { get {} set {} }
  }

  let c = C()
  let one: Int = 1
  // Should choose the settable subscript to find a problem with, not the get-only subscript
  c[one] = 2.5 // expected-error {{cannot convert value of type 'Int' to expected argument type 'Double'}}
}
