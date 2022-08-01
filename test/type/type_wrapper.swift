// RUN: %target-typecheck-verify-swift

@typeWrapper
struct ConcreteTypeWrapper { // expected-error {{type wrapper has to declare a single generic parameter for underlying storage type}}
  init(memberwise: Int) {}
}

@typeWrapper
struct EmptyTypeWrapper<S> { // expected-error {{type wrapper type 'EmptyTypeWrapper' does not contain a required initializer - init(memberwise:)}}
}

@typeWrapper
struct NoMemberwiseInit<S> {
  // expected-error@-1 {{type wrapper type 'NoMemberwiseInit' does not contain a required initializer - init(memberwise:)}}

  subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

@typeWrapper
struct FailableInit<S> {
  init?(memberwise: S) { // expected-error {{type wrapper initializer 'init(memberwise:)' cannot be failable}}
  }

  subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

// Okay because there is a valid `init(memberwise:)` overload.
@typeWrapper
struct FailableAndValidInit<S> {
  init(memberwise: S) {
  }

  init?(memberwise: S) where S == Int {
  }

  subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

@typeWrapper
public struct InaccessibleInit<S> {
  fileprivate init(memberwise: S) {
    // expected-error@-1 {{fileprivate initializer 'init(memberwise:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleInit' (which is public)}}
  }

  private init?(memberwise: S) where S: AnyObject {
    // expected-error@-1 {{private initializer 'init(memberwise:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleInit' (which is public)}}
    // expected-error@-2 {{type wrapper initializer 'init(memberwise:)' cannot be failable}}
  }

  subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

@typeWrapper
struct NoSubscripts<S> {
  // expected-error@-1 {{type wrapper type 'NoSubscripts' does not contain a required subscript - subscript(storedKeyPath:)}}
  init(memberwise: S) {}
}

@typeWrapper
struct InaccessibleOrInvalidSubscripts<S> {
  init(memberwise: S) {}

  fileprivate subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    // expected-error@-1 {{fileprivate subscript 'subscript(storageKeyPath:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleOrInvalidSubscripts' (which is internal)}}
    get { fatalError() }
  }

  private subscript<V>(storageKeyPath path: KeyPath<S, V>) -> [V] {
    // expected-error@-1 {{private subscript 'subscript(storageKeyPath:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleOrInvalidSubscripts' (which is internal)}}
    get { fatalError() }
  }

  subscript(storageKeyPath path: Int) -> Bool { // expected-error {{type wrapper subscript expects a key path parameter type (got: 'Int')}}
    get { true }
  }
}

@typeWrapper
struct NoopWrapper<S> {
  init(memberwise: S) {}

  subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
    set { }
  }
}

@NoopWrapper
struct A {
  var a: String
  var b: Int
}

@NoopWrapper
class GenericA<K: Hashable, V> {
  var data: [K: V]
}

@NoopWrapper // expected-error {{type wrapper attribute 'NoopWrapper' can only be applied to a class, struct}}
protocol P {
}

@NoopWrapper // expected-error {{type wrapper attribute 'NoopWrapper' can only be applied to a class, struct}}
enum E {
  var x: Int { get { 42 } }
}

func testWrappedTypeAccessChecking() {
  let a = A(a: "", b: 42) // synthesized init
  let b = GenericA(data: ["ultimate question": 42]) // generic synthesized init

  _ = a.a // Ok
  _ = b.data // Ok
}

struct Parent {
  @typeWrapper
  struct Wrapper<S> {
    init(memberwise: S) {}

    subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
      get { fatalError() }
      set { }
    }
  }
}

func testLocalWithNestedWrapper() {
  @Parent.Wrapper
  class WithNestedWrapper<T> {
    var test: [T]

    var computed: String {
      get { "" }
    }
  }

  let t = WithNestedWrapper(test: [1, 2]) // synthesized init
  _ = t.test // Ok
  _ = t.computed // Ok
}
