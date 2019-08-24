public class BaseClass {
  public func memberFunc1() -> Int { return 1; }
  public func memberFunc2() -> Int { return 1; }
  public func memberFunc3() -> Int { return 1; }
  public func memberFunc4() -> Int { return 1; }
  public func memberFunc5() -> Int { return 1; }
  public typealias memberType1 = Int
  public typealias memberType2 = Int
  public typealias memberType3 = Int
  public typealias memberType4 = Int
  public typealias memberType5 = Int
}

public class DerivedClass : BaseClass {
  public func derivedMemberFunc1() -> Int { return 1; }
  public func derivedMemberFunc2() -> Int { return 1; }
  public func derivedMemberFunc3() -> Int { return 1; }
  public func derivedMemberFunc4() -> Int { return 1; }
  public func derivedMemberFunc5() -> Int { return 1; }
  public typealias derivedMemberType1 = Int
  public typealias derivedMemberType2 = Int
  public typealias derivedMemberType3 = Int
  public typealias derivedMemberType4 = Int
  public typealias derivedMemberType5 = Int
}

public class OverrideDerivedClass : BaseClass {
  override public func memberFunc1() -> Int { return 1; }
  override public func memberFunc2() -> Int { return 1; }
  override public func memberFunc3() -> Int { return 1; }
  override public func memberFunc4() -> Int { return 1; }
  override public func memberFunc5() -> Int { return 1; }
}

public struct BaseStruct {
  public func memberFunc1() -> Int { return 1; }
  public func memberFunc2() -> Int { return 1; }
  public func memberFunc3() -> Int { return 1; }
  public func memberFunc4() -> Int { return 1; }
  public func memberFunc5() -> Int { return 1; }
  public func memberFunc6() -> Int { return 1; }
  public func memberFunc7() -> Int { return 1; }
  public func memberFunc8() -> Int { return 1; }
  public func memberFunc9() -> Int { return 1; }
  public init() {}
}

public enum BaseEnum {
  case member1
  case member2
  case member3
  case member4
  case member5
}

public protocol BaseProto {
  func protoMemberFunc1() -> Int
  func protoMemberFunc2() -> Int
  func protoMemberFunc3() -> Int
  func protoMemberFunc4() -> Int
  func protoMemberFunc5() -> Int
}

public struct BaseExt {
  public func memberFunc1() -> Int { return 1; }
  public func memberFunc2() -> Int { return 1; }
  public func memberFunc3() -> Int { return 1; }
  public func memberFunc4() -> Int { return 1; }
  public func memberFunc5() -> Int { return 1; }
  public typealias memberType1 = Int
  public typealias memberType2 = Int
  public typealias memberType3 = Int
  public typealias memberType4 = Int
  public typealias memberType5 = Int
}

extension BaseExt {
  public func extMemberFunc1() -> Int { return 1; }
  public func extMemberFunc2() -> Int { return 1; }
  public func extMemberFunc3() -> Int { return 1; }
  public func extMemberFunc4() -> Int { return 1; }
  public func extMemberFunc5() -> Int { return 1; }
  public typealias extMemberType1 = Int
  public typealias extMemberType2 = Int
  public typealias extMemberType3 = Int
  public typealias extMemberType4 = Int
  public typealias extMemberType5 = Int
}

