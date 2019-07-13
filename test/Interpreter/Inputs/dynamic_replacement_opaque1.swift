protocol P {
  func myValue() -> Int64
}

extension Int64: P {
  public func myValue() -> Int64 {
    return self
  }

}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
func bar(_ x: Int64) -> some P {
  return x
}

struct Container {
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  func bar(_ x: Int64) -> some P {
    return x
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  var computedProperty : some P {
    get {
      return Int64(2)
    }
    set {
      print("original \(newValue)")
    }
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  subscript(_ x: Int) -> some P {
    get {
      return Int64(2)
    }
    set {
      print("original \(newValue)")
    }
  }
}

protocol Q {}

struct NewType : Q {}

extension Int64 : Q {}

public protocol Assoc {
  associatedtype A = Int
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  func act() -> A
}

struct Test : Assoc {
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  func act() -> some Q {
    return Int64(1)
  }
}
