private struct Base {
  private func bar() {}
}

struct Value {
}

extension Value {
  fileprivate func foo() {}
}
