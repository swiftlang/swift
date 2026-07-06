// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/56254

protocol OptionalType {
  associatedtype Wrapped
  var wrapped: Wrapped? { get }
}

protocol DefaultsBridge {
  associatedtype T
}

protocol DefaultsSerializable {
  typealias T = Bridge.T
  associatedtype Bridge: DefaultsBridge
}

protocol DefaultsKeyStore {}

struct DefaultsKeys: DefaultsKeyStore {}
struct DefaultsKey<ValueType> {}

@dynamicMemberLookup
struct DefaultsAdapter<KeyStore: DefaultsKeyStore> {
  @available(*, unavailable)
  subscript(dynamicMember member: String) -> Never {
    fatalError()
  }

  subscript<T: DefaultsSerializable>(keyPath: KeyPath<KeyStore, DefaultsKey<T>>) -> T.T where T.T == T {
    get { fatalError() }
    set { fatalError() }
  }

  subscript<T: DefaultsSerializable>(dynamicMember keyPath: KeyPath<KeyStore, DefaultsKey<T>>) -> T.T where T: OptionalType, T.T == T {
    get { fatalError() }
    set { fatalError() }
  }
}


var Defaults = DefaultsAdapter<DefaultsKeys>()
Defaults[\.missingKey] = "" // expected-error {{}}
