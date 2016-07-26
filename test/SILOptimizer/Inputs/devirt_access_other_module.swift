public class ExternalClass {
  fileprivate func foo() {}
}

public func getExternalClass() -> ExternalClass {
  return ExternalClass()
}

// Note: This will eventually be illegal (to have a public @_transparent function
// referring to a private method), but for now it lets us test what can and
// can't be optimized.
@_transparent public func invokeFoo(_ obj: ExternalClass) {
  obj.foo()
}

private class PrivateSubclass : ExternalClass {
  override private func foo() {}
}
