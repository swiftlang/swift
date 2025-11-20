public protocol P {
  func hello()
}

public struct Empty : P {
  public func hello() { print("Hello from Empty") }
}

public struct Tuple<T>: P {
  public init(_ tuple: T) {}

  public func hello() { print("Hello from Tuple") }
}

@available(macOS 10.15, *)
struct Named : P {
  public func hello() { print("Hello from Named") }
}

@resultBuilder
public struct Example {
  public static func buildOptional<T: P>(_ v: T?) -> some P {
    if #available(macOS 100.0.1, *) {
      let result = v!
      result.hello()
      return result
    } else {
      let result = Empty()
      result.hello()
      return result
    }
  }

  public static func buildLimitedAvailability<T: P>(_ component: T) -> some P {
    component
  }

  public static func buildBlock<T: P>(_ components: T) -> T {
    return components
  }

  public static func buildBlock<T1: P, T2: P>(_ v1: T1, _ v2: T2) -> Tuple<(T1, T2)> {
    return Tuple((v1, v2))
  }
}

public func testAvailableQueryWithUniversalResult() -> some P {
  if #available(macOS 100.0.1, *) {
    return Tuple<(Int, Int)>((0, 0))
  }

  return Empty()
}

public func testUnavailableQueryWithLimitedResult() -> some P {
  if #unavailable(macOS 100.0.1) {
    return Tuple<(Int, Int)>((0, 1))
  }

  return Empty()
}

public func testAvailableQueryWithLimitedResult() -> some P {
  if #available(macOS 10.15, *) {
    return Named()
  }

  return Tuple<(String, Int)>(("", 0))
}

public func testInactiveAvailableQuery() -> some P {
  if #available(iOS 50, *) {
    return Empty()
  }

  return Named()
}
