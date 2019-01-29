import A

extension Impl {
  @_dynamicReplacement(for: foo())
  func repl() -> Int {
    return foo() + 1
  }
}
