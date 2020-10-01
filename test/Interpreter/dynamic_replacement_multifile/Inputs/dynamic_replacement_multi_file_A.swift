dynamic func replaceable1() -> Int {
   return 0
}

@_dynamicReplacement(for: replaceable1())
func replaceable1_r() -> Int {
  return 2
}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
dynamic func bar1(_ x: Int) -> some P {
  return x
}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
@_dynamicReplacement(for: bar1(_:))
func bar1_r(_ x: Int) -> some P {
  return Pair()
}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
dynamic func bar2(_ x: Int) -> some P {
  return x
}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
@_dynamicReplacement(for: bar2(_:))
func bar2_r(_ x: Int) -> some P {
  return Pair()
}

@_dynamicReplacement(for: replaceableInOtherFile())
func replaceableInOtherFile_r() -> Int {
  return 7
}
