extension B {
  public init?<T : A>(_: T) where T.T == Self {
    return nil
  }
}

public class SubProtoImpl30984417: SubProto30984417 {
  public func toConcrete() -> SubProtoImpl30984417 { return self }
}
