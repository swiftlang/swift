private protocol HasDefaultFoo {}

extension HasDefaultFoo {
  private func foo() {}
}

private class Base {
  func foo() {}
}
