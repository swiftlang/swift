public class ExternalClass {
  private func foo() {}
}

public func getExternalClass() -> ExternalClass {
  return ExternalClass()
}

// Note: This will eventually be illegal (to have a public @transparent function
// referring to a private method), but for now it lets us test what can and
// can't be optimized.
@transparent public func invokeFoo(obj: ExternalClass) {
  obj.foo()
}

private class PrivateSubclass : ExternalClass {
  override private func foo() {}
}
