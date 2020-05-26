dynamic func replaceable2() -> Int {
   return 0
}

@_dynamicReplacement(for: replaceable2())
func replaceable2_r() -> Int {
  return 3
}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
dynamic func bar3(_ x: Int) -> some P {
  return x
}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
@_dynamicReplacement(for: bar3(_:))
func bar3_r(_ x: Int) -> some P {
  return Pair()
}
