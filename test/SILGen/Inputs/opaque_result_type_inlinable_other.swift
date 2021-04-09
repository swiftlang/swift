fileprivate struct PrivateS {}

public func callee() -> some Any {
  return PrivateS()
}
