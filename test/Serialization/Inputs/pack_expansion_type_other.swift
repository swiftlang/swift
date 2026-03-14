public func variadicFunction<each T, each U>(t: repeat each T, u: repeat each U) -> (repeat (each T, each U)) {
  // FIXME: return (repeat (each t, each u))
  fatalError()
}

public struct VariadicType<each T> {
  public init() {}

  public func variadicMethod<each U>(t: repeat each T, u: repeat each U) -> (repeat (each T, each U)) {
    return (repeat (each t, each u))
  }
}
