dynamic func replaceable2() -> Int {
   return 0
}

@_dynamicReplacement(for: replaceable2())
func replaceable2_r() -> Int {
  return 3
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
dynamic func bar3(_ x: Int) -> some P {
  return x
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
@_dynamicReplacement(for: bar3(_:))
func bar3_r(_ x: Int) -> some P {
  return Pair()
}
