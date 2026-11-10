// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/91859

// Referencing a property from an extension whose requirements conflict with
// each other used to crash while diagnosing the failed requirement.

protocol P {}
struct C: P {}

struct G<T: P> {}

extension G where T == Int {
// expected-error@-1 {{no type for 'T' can satisfy both 'T == Int' and 'T : P'}}
// expected-note@-2 2 {{where 'T' = 'Int'}}
  var x: Int { 0 }
  var y: Int { x }
  // expected-error@-1 {{property 'x' requires that 'Int' conform to 'P'}}

  static var s: Int { 0 }
  static var t: Int { s }
  // expected-error@-1 {{static property 's' requires that 'Int' conform to 'P'}}
}

extension G where T == G<C> {
// expected-error@-1 {{no type for 'T' can satisfy both 'T == G<C>' and 'T : P'}}
// expected-note@-2 {{where 'T' = 'G<C>'}}
  var a: Int { 0 }
  var b: Int { a }
  // expected-error@-1 {{property 'a' requires that 'G<C>' conform to 'P'}}

  func f() {} // expected-note {{where 'T' = 'G<C>'}}
  func g() {
    f() // expected-error {{instance method 'f()' requires that 'G<C>' conform to 'P'}}
  }
}
