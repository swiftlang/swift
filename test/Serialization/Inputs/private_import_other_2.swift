private struct Base {
  private func member() {}
  private func bar() {}
}

private struct Other {
}

extension Value {
  fileprivate func foo() {}
}
