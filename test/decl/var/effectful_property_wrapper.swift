// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

// Currently, we don't support having property wrappers that are effectful.
// Eventually we'd like to add this.

@propertyWrapper
struct Abstraction<T> {
    private var value : T

    init(_ initial : T) { self.value = initial }

    var wrappedValue : T {
        get throws { return value } // expected-error{{property wrapper's 'wrappedValue' property cannot define an 'async' or 'throws' getter}}
    }

    // its OK to have effectful props that are not `wrappedValue`

    var prop1 : T {
      get async { value }
    }
    var prop2 : T {
      get throws { value }
    }
    var prop3 : T {
      get async throws { value }
    }
}

@propertyWrapper
struct NeedlessIntWrapper {
  var wrappedValue : Int {
    get {}
  }
}

struct S {
  // expected-error@+1 {{property wrapper cannot be applied to a computed property}}
  @NeedlessIntWrapper var throwingProp : Int {
    get throws { 0 }
  }
}
