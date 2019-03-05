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
    public private(set) var publicVarWithPrivateSetter : Int = 0
    public fileprivate(set) var publicVarWithFilePrivateSetter : Int = 0
}
