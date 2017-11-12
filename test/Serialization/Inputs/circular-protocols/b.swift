public protocol BB {
  associatedtype T
}

public protocol B {
  associatedtype T : BB
}

public protocol BaseProto30984417 {
  func toConcrete() -> SubProtoImpl30984417
}