@_private(sourceFile: "TestOpaque1.swift") import TestOpaque1

struct Pair : P {
  var x = 0
  var y = 1
  func myValue() -> Int{
    return y
  }
}

@_dynamicReplacement(for:bar(_:))
func _replacement_bar(y x: Int) -> some P {
  return Pair()
}

extension Container {
  @_dynamicReplacement(for:bar(_:))
  func _replacement_bar(y x: Int) -> some P {
    return Pair()
  }

  @_dynamicReplacement(for: computedProperty)
  var _replacement_computedProperty : some P {
     get {
       return Pair()
     }
     set {
      print("replacement \(newValue)")
     }
  }
  @_dynamicReplacement(for: subscript(_:))
  subscript(y x: Int) -> some P {
    get {
      return Pair()
    }
    set {
      print("replacement \(newValue)")
    }
  }
}
