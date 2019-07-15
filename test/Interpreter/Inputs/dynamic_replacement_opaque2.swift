@_private(sourceFile: "TestOpaque1.swift") import TestOpaque1

struct Pair : P {
  var x = Int64(0)
  var y = Int64(1)
  func myValue() -> Int64 {
    return y
  }
}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
@_dynamicReplacement(for:bar(_:))
func _replacement_bar(y x: Int64) -> some P {
  return Pair()
}

extension Container {
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  @_dynamicReplacement(for:bar(_:))
  func _replacement_bar(y x: Int64) -> some P {
    return Pair()
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  @_dynamicReplacement(for: computedProperty)
  var _replacement_computedProperty : some P {
     get {
       return Pair()
     }
     set {
      print("replacement \(newValue)")
     }
  }

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
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

extension Test {
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  @_dynamicReplacement(for: act)
  func act_r() -> some Q {
    return NewType()
  }
}
