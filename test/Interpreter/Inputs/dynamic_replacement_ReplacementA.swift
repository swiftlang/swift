@_weakLinked import LibA

extension A {
  @_dynamicReplacement(for: printThis())
  func _replacementForPrintThis() {
    Swift.print("replacement")
  }
}
