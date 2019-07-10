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

@available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
public func resilientFunction() -> some P {
#if BEFORE
  return Int(5)
#else
  return Pair()
#endif
}

@available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
public func expectedResult() -> Int {
  return resilientFunction().getValue()
}

@available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
public func expectedSize() -> Int {
  return MemoryLayout.size(ofValue: resilientFunction())
}


public struct Container {
  public init() {}

  @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
  public var property : some P {
    get {
#if BEFORE
      return Int(5)
#else
      return Pair()
#endif
    }
  }

  @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
  public func expectedResult() -> Int {
    return property.getValue()
  }

  @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
  public func expectedSize() -> Int {
    return MemoryLayout.size(ofValue: property)
  }
}
