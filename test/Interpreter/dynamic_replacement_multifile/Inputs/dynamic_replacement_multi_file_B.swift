dynamic func replaceable2() -> Int {
   return 0
}

@_dynamicReplacement(for: replaceable2())
func replaceable2_r() -> Int {
  return 3
}

@available(SwiftStdlib 5.1, *)
dynamic func bar3(_ x: Int) -> some P {
  return x
}

@available(SwiftStdlib 5.1, *)
@_dynamicReplacement(for: bar3(_:))
func bar3_r(_ x: Int) -> some P {
  return Pair()
}
