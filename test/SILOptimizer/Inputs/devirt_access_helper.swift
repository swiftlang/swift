class InternalClass {
  private func foo() {}
  internal func bar() {}
}

// Marked @inline(never) to keep from devirtualizing based on this.
@inline(never) func getInternalClass() -> InternalClass {
  return InternalClass()
}

@_transparent func invokeFoo(_ obj: InternalClass) {
  obj.foo()
}

// Note that neither 'foo' nor 'bar' are ever overridden.
private class PrivateSubclass : InternalClass {}
