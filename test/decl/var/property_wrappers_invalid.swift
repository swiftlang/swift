// RUN: %target-swift-frontend -typecheck %s -verify

public class Store {
    @Published public var state: Any
    init() {
      self.state = 0
    }
}

@propertyWrapper public struct Published<Value> {
  public init(wrappedValue: Value) {}
  public var wrappedValue: Value {
    get { fatalError() }
    set {}
  }
  public static subscript(_enclosingInstance object: Any,
                          wrapped wrappedKeyPath: Any, // expected-error {{parameter 'wrapped' of enclosing-self subscript should have either 'WritableKeyPath<...>' or 'ReferenceWritableKeyPath<...>' type (got 'Any')}}
                          storage storageKeyPath: Any) // expected-error {{parameter 'storage' of enclosing-self subscript should have either 'WritableKeyPath<...>' or 'ReferenceWritableKeyPath<...>' type (got 'Any')}}
      -> Value {
    get { fatalError() }
    set {}
  }
  public struct Publisher {}
  public var projectedValue: Publisher {
    mutating get { fatalError() }
  }
}
