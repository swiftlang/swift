// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
  func f() -> A
}

struct S: P {
  func f() -> some Any { return 123 }
}

let x1: S.A = "hello world"
// expected-error@-1 {{cannot convert value of type 'String' to specified type 'S.A' (aka 'some Any')}}

let x2: S.A = { return "hello world" }()
// expected-error@-1 {{cannot convert value of type 'String' to closure result type 'S.A' (aka 'some Any')}}
let x3: () -> S.A = { return "hello world" }
// expected-error@-1 {{cannot convert value of type 'String' to closure result type 'S.A' (aka 'some Any')}}

let x4: some Any = 123
let x5: some Any = { return 123 }()

// FIXME: This should work
let x6: () -> some Any = { return 123 }
// expected-error@-1 {{cannot convert value of type 'Int' to closure result type 'some Any'}}
