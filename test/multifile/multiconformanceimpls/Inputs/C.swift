import A

extension Impl : Proto {
  public func wellNow() {
    print("Impl conformance in C")
  }
}

public func checkAnyInC() {
  if let container = any as? Container<Impl> {
    container.contained.wellNow()
  } else {
    print("check returned not Container<Impl> in C")
  }
}
