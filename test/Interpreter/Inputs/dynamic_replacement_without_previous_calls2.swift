import Module1

@_dynamicReplacement(for: selfRec(_: _:))
func selfRec_r(_ x: Int, _ acc: Int) -> Int {
  if x <= 0 {
    return acc
  }
  return selfRec(x - 1, acc + 1)
}

extension AStruct {
  @_dynamicReplacement(for: selfRec(_: _:))
  func selfRec_r(_ x: Int, _ acc: Int) -> Int {
    if x <= 0 {
      return acc
    }
    return selfRec(x - 1, acc + 1)
  }
}

extension AClass {
  @_dynamicReplacement(for: selfRec(_: _:))
  func selfRec_r(_ x: Int, _ acc: Int) -> Int {
    if x <= 0 {
      return acc
    }
    return selfRec(x - 1, acc + 1)
  }
}
