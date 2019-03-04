private struct Base {
  private func bar() {}
}

struct Value {
}

extension Value {
  fileprivate func foo() {}
}

struct Internal {
    private(set) var internalVarWithPrivateSetter : Int = 0
    fileprivate(set) var internalVarWithFilePrivateSetter : Int = 0
}
