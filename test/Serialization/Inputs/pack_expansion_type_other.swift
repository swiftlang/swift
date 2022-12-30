public func variadicFunction<T..., U...>(t: T..., u: U...) -> ((T, U)...) {
  // FIXME: return ((t, u)...)
  fatalError()
}

public struct VariadicType<T...> {
  public func variadicMethod<U...>(t: T..., u: U...) -> ((T, U)...) {
    // FIXME: return ((t, u)...)
    fatalError()
  }
}
