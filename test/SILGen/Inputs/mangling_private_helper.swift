open class Base {
  fileprivate func privateMethod() {}
}

// Demonstrate the need for a vtable entry for privateMethod().
// This isn't strictly necessary.
private class Subclass : Base {
  override private func privateMethod() {}
}
