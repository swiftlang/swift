// RUN: %target-typecheck-verify-swift

protocol P {
  call(x: Self)
}

struct ConcreteType {
  call<T, U>(_ x: T, _ y: U) -> (T, U) {
    return (x, y)
  }

  call<T, U>(_ fn: @escaping (T) -> U) -> (T) -> U {
    return fn
  }
}

struct GenericType<T : Collection> {
}

let concrete = ConcreteType()
_ = concrete(1, 3.0)
_ = concrete(concrete, concrete.call as ([Int], Float) -> ([Int], Float))

func genericContext<T, U>(_ x: T, _ y: U) {
  _ = concrete(x, x)
  _ = concrete(x, y)
}
