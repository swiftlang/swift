struct OneArgument<First> {
  let first: First

  let line: UInt

  init(_ first: First, line: UInt = #line) {
    self.first = first
    self.line = line
  }
}

extension OneArgument : CustomStringConvertible {
  var description: String {
    "\(first) @ \(line)"
  }
}
