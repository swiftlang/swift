protocol P {
  func myValue() -> Int
}

extension Int: P {
  public func myValue() -> Int {
    return self
  }
}

func bar(_ x: Int) -> some P {
  return x
}

struct Container {
  func bar(_ x: Int) -> some P {
    return x
  }
}
