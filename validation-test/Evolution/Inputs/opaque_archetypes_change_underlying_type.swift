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

public func resilientFunction() -> some P {
#if BEFORE
  return Int(5)
#else
  return Pair()
#endif
}

public func expectedResult() -> Int {
  return resilientFunction().getValue()
}

public func expectedSize() -> Int {
  return MemoryLayout.size(ofValue: resilientFunction())
}


public struct Container {
  public init() {}

  public var property : some P {
    get {
#if BEFORE
      return Int(5)
#else
      return Pair()
#endif
    }
  }

  public func expectedResult() -> Int {
    return property.getValue()
  }

  public func expectedSize() -> Int {
    return MemoryLayout.size(ofValue: property)
  }
}
