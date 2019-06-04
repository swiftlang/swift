protocol P {
  func myValue() -> Int
}

extension Int: P {
  public func myValue() -> Int {
    return self
  }

}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
func bar(_ x: Int) -> some P {
  return x
}

struct Container {
  @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  func bar(_ x: Int) -> some P {
    return x
  }

  @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  var computedProperty : some P {
    get {
      return 2
    }
    set {
      print("original \(newValue)")
    }
  }

  @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  subscript(_ x: Int) -> some P {
    get {
      return 2
    }
    set {
      print("original \(newValue)")
    }
  }
}

protocol Q {}

struct NewType : Q {}

extension Int : Q {}

public protocol Assoc {
  associatedtype A = Int
  @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  func act() -> A
}

struct Test : Assoc {
  @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  func act() -> some Q {
    return 1
  }
}
