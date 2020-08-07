public struct OneArgument<First : P> {
  public let first: First

  public let line: UInt

  public init(_ first: First, line: UInt = #line) {
    self.first = first
    self.line = line
  }
}

extension OneArgument : CustomStringConvertible {
  public var description: String {
    "\(first) @ \(line)"
  }
}


