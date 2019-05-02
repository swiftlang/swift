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

  var computedProperty : some P {
    get {
      return 2
    }
    set {
      print("original \(newValue)")
    }
  }

  subscript(_ x: Int) -> some P {
    get {
      return 2
    }
    set {
      print("original \(newValue)")
    }
  }
}
