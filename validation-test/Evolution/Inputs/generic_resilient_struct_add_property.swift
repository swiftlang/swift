
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public protocol P {
  static func compute() -> Int
}

public struct A : P {
  public init() {}

  public static func compute() -> Int {
    return 42
  }
}

#if BEFORE

public struct S<T: P> {
  public init() {}
}

#else

public struct S<T: P> {
  public init() {
    question = "Ultimate Question"
  }

  public var question: String
}

#endif

