// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

// Parsing an UnresolvedSpecializeExpr containing a PackExpansionType
struct G<each T> {}

func f<each T>(_: repeat each T) {
  _ = G< >.self
  _ = G<Int>.self
  _ = G<Int, String>.self
  _ = G<repeat each T>.self
  _ = G<Int, repeat Array<each T>>.self
}

// Forming PackExpansionTypeReprs in simplifyTypeExpr()
func g<each T>(_: repeat each T) {
  _ = (repeat each T).self
  _ = (Int, repeat each T).self
  _ = ((repeat each T) -> ()).self
  _ = ((Int, repeat Array<each T>) -> ()).self

  _ = (repeat each Int).self
  // expected-error@-1 {{pack expansion 'Int' must contain at least one pack reference}}
  // expected-error@-2 {{'each' cannot be applied to non-pack type 'Int'}}{{15-19=}}
}

struct MissingMemberError<each T> {
  init() {
    self.doesNotExist = 1
    // expected-error@-1 {{value of type 'MissingMemberError<repeat each T>' has no member 'doesNotExist'}}
  }
}

// https://github.com/apple/swift/issues/66095
do {
  struct Test<each S> {
    init(_ s: repeat each S) {}
  }

  func test1<each T>(_ v: repeat each T) -> Test<repeat each T> {
    return Test(repeat each v) // Ok
  }

  func test2<each T>(_ v: repeat each T) -> Test<repeat each T> {
    return Test<repeat each T>(repeat each v) // Ok
  }

  func test3<each T>(_ v: repeat each T) -> Test<String, repeat each T, Int> {
    return Test("a", repeat each v, 42) // Ok
  }

  func test4<each T>(_ v: repeat each T) -> Test<repeat each T, String, Int> {
    return Test<repeat each T, String, Int>(repeat each v, "a", 42) // Ok
  }

  func test5<each T>(_ v: repeat each T) -> Test<String, Int, repeat each T> {
    return Test<String, Int, repeat each T>("a", 42, repeat each v) // Ok
  }
}

// rdar://107479662 - variadic tuple of Sendable elements does not conform to Sendable
do {
  struct Test<each T> : Sendable {
    let prop: (repeat TestProperty<each T>)
  }

  struct TestProperty<T> : Sendable {
  }
}

// https://github.com/apple/swift/issues/68160
// https://github.com/apple/swift/issues/69390
do {
  struct G<each T, U> {
    let f: (repeat Optional<each T>) -> U
    var f2: (repeat each T) -> Void

    init(f: @escaping (repeat Optional<each T>) -> U) {
      self.f = f
    }

    func foo() {
      let _: (repeat each T) -> Void = f2
    }
  }
}

// rdar://121692664 - compiler doesn't respect contravariance of the variadic function parameters
do {
  class Value<T> {
    init<each V>(_ v: repeat Value<each V>,
                 transform: @escaping (repeat each V) -> T) {
    }
  }

  func coerce(_: Int) {}

  func test(first: Value<Int?>, second: Value<(a: Int, b: Int)>) {
    let _ = Value(first, second) { first, second in
      coerce(first)
      // expected-error@-1 {{value of optional type 'Int?' must be unwrapped to a value of type 'Int'}}
      // expected-note@-2 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
      // expected-note@-3 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
    }

    // multi-statement closure
    let _ = Value(first, second) { first, second in
      _ = 42
      coerce(first)
      // expected-error@-1 {{value of optional type 'Int?' must be unwrapped to a value of type 'Int'}}
      // expected-note@-2 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
      // expected-note@-3 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
    }
  }
}

// apple/swift#69432 - Passing nil to a parameter pack fails to produce diagnostic for expression
do {
  struct Foo<each T> {
    init(_ value: repeat each T) {}
    // expected-note@-1 {{in inferring pack element #0 of 'value'}}
    // expected-note@-2 {{in inferring pack element #0 of 'value'}}
  }

  _ = Foo(nil) // expected-error {{'nil' requires a contextual type}}
  _ = Foo(nil, 1) // expected-error {{'nil' requires a contextual type}}
}
