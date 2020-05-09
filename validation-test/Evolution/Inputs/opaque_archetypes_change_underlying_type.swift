public protocol P {
  func getValue() -> Int
}

extension Int : P {
  public func getValue() -> Int {
    return self + 1
  }
}

#if !BEFORE
public struct Pair : P {
  var x = 0
  var y = 1

  public func getValue() -> Int {
    return y
  }
}
#endif

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
public func resilientFunction() -> some P {
#if BEFORE
  return Int(5)
#else
  return Pair()
#endif
}

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
public func expectedResult() -> Int {
  return resilientFunction().getValue()
}

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
public func expectedSize() -> Int {
  return MemoryLayout.size(ofValue: resilientFunction())
}


public struct Container {
  public init() {}

  @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
  public var property : some P {
    get {
#if BEFORE
      return Int(5)
#else
      return Pair()
#endif
    }
  }

  @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
  public func expectedResult() -> Int {
    return property.getValue()
  }

  @available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
  public func expectedSize() -> Int {
    return MemoryLayout.size(ofValue: property)
  }
}
