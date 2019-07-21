public struct SpecialInt {
  public var value = 0
  public init() {}
}

prefix operator +++
postfix operator +++

prefix public func +++(base: inout SpecialInt) {
  base.value += 2
}

postfix public func +++(base: inout SpecialInt) {
}

