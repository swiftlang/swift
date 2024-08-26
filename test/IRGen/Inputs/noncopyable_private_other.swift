fileprivate struct PrivateType : ~Copyable {
  let x = 5

  deinit {
    print("deinit")
  }
}

public struct PublicType : ~Copyable {
  fileprivate let p = PrivateType()

  public init() {}
}

public enum PublicSinglePayloadEnum : ~Copyable {
    case empty
    case some(PrivateType)
}

public enum PublicMultiPayloadEnum : ~Copyable {
    case empty
    case some(PrivateType)
    case someOther(PrivateType)
}
