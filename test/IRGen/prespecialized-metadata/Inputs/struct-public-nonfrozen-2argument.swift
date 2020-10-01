public struct TwoArgument<First, Second> {
  public let first: First
  public let second: Second

  public let line: UInt

  public init(_ first: First, _ second: Second, line: UInt = #line) {
    self.first = first
    self.second = second
    self.line = line
  }
}

extension TwoArgument : CustomStringConvertible {
  public var description: String {
    "(\(first), \(second)) @ \(line)"
  }
}

