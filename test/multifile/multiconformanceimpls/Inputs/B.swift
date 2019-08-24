import A

extension Impl : Proto {
  public func wellNow() {
    print("Impl conformance in B")
  }
}

public func setAnyWithImplFromB() {
  any = Container(Impl())
}
