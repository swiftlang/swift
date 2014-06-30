@public struct SpecialInt {
  @public var value = 0
}

operator prefix +++ {}
operator postfix +++ {}

@prefix @public func +++(inout base: SpecialInt) {
  base.value += 2
}

@postfix @public func +++(inout base: SpecialInt) {
  println("use the prefix form instead")
}

