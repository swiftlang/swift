// Things in this file are deliberately internal. The test harness uses @testable import.

internal class Base : Printable {
  let id: Int

  init(_ id: Int) {
    self.id = id
  }
  
  var description: String { return "instance \(id)" }
  
  private func privateFn() -> String {
    return "private \(id)"
  }
  func callPrivate() -> String {
    return privateFn()
  }
}

private class PrivateSub : Base {
  override func privateFn() -> String {
    return "really private"
  }
}

internal func getPrivateInstance() -> Base {
  return PrivateSub(0)
}
