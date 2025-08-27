// RUN: %target-swift-frontend -typecheck %s -verify -verify-ignore-unknown

// FIXME: This should produce a diagnostic with a proper
// source location. Right now, we just get these errors:

// <unknown>:0: error: cannot infer key path type from context; consider explicitly specifying a root type
// <unknown>:0: error: cannot infer key path type from context; consider explicitly specifying a root type
// <unknown>:0: error: cannot infer key path type from context; consider explicitly specifying a root type
// <unknown>:0: error: cannot infer key path type from context; consider explicitly specifying a root type
// <unknown>:0: error: cannot infer key path type from context; consider explicitly specifying a root type
// <unknown>:0: error: cannot infer key path type from context; consider explicitly specifying a root type

// The actual problem is the type of the subscript declaration is wrong.

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
                          wrapped wrappedKeyPath: Any,
                          storage storageKeyPath: Any)
      -> Value {
    get { fatalError() }
    set {}
  }
  public struct Publisher {}
  public var projectedValue: Publisher {
    mutating get { fatalError() }
  }
}
