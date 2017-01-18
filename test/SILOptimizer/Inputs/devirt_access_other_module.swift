public class ExternalClass {
  fileprivate func foo() {}
}

public func getExternalClass() -> ExternalClass {
  return ExternalClass()
}

private class PrivateSubclass : ExternalClass {
  override fileprivate func foo() {}
}
