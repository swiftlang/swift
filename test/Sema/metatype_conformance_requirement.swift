// RUN: %target-swift-frontend -typecheck -verify %s

protocol P {
  var identifier: Int { get }
  func describe() -> String
}

protocol Q: P {
}

// expected-note@+1{{required by global function 'property' where 'T.Type' = 'S.Type'}}
func property<T>(_: T.Type) -> Int where T.Type: P {
  T.identifier
}

func method<T>(_: T.Type) -> String where T.Type: P {
  T.describe()
}

func forward<T>(_ t: T.Type) -> Int where T.Type: P {
  property(t)
}

func refined<T>(_: T.Type) -> String where T.Type: Q {
  T.describe()
}

func unknown<T>(_: T.Type) where T.Type: P {
  // expected-error@+1{{type 'T' has no member 'nonexistent'}}
  _ = T.nonexistent
}

func unrequired<T>(_: T.Type) {
  // expected-error@+1{{type 'T' has no member 'identifier'}}
  _ = T.identifier
}

struct S {
}

func f() {
  // expected-error@+2{{type 'S.Type' cannot conform to 'P'}}
  // expected-note@+1{{only concrete types such as structs, enums and classes can conform to protocols}}
  _ = property(S.self)
}

func packExpansion<each T>(_: repeat (each T).Type) -> (repeat (each T)?)
    where repeat (each T).Type: P {
  var index = 0

  func unpack<U>(_: U.Type) -> U? where U.Type: P {
    defer { index += 1 }
    return nil
  }

  return (repeat unpack((each T).self))
}

func packIteration<each T>(_ types: repeat (each T).Type)
    where repeat (each T).Type: P {
  for type in repeat each types {
    _ = type.identifier
  }
}

struct Generic<Value> {
  init(_ value: Value) {}
}

func construct<Value>(_ value: Value) {
  _ = Generic<Value>(value)
}
