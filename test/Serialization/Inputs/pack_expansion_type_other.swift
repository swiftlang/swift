public func variadicFunction<T..., U...>(t: repeat each T, u: repeat each U) -> (repeat (each T, each U)) {
  // FIXME: return (repeat (each t, each u))
  fatalError()
}

public struct VariadicType<T...> {
  public func variadicMethod<U...>(t: repeat each T, u: repeat each U) -> (repeat (each T, each U)) {
    // FIXME: return (repeat (each t, each u))
    fatalError()
  }
}
