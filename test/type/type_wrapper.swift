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
