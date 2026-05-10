public let global: Int = 0

#if CONCURRENCY
@globalActor
public struct GA {
  public actor A {}
  public static let shared = A()
}
#endif

public func function(with argument: Int) {}
public func throwingFunc() throws {}
public func genericFunc<T: P>(_ t: T) {}
public func funcWithOpaqueResult() -> some P { return S(member: 0) }
@_cdecl("cdecl_func") public func cdeclFunc() {}
@_silgen_name("forward_declared_func") public func forwardDeclaredFunc()
#if CONCURRENCY
@GA public func isolatedFunc() {}
public func asyncFunc() async {}
#endif

public dynamic func dynamicFunc() {}
@_dynamicReplacement(for: dynamicFunc) public func replacementFunc() {}

public dynamic func dynamicFuncOpaqueResult() -> some P { return S(member: 0) }
@_dynamicReplacement(for: dynamicFuncOpaqueResult)
public func replacementFuncOpaqueResult() -> some P { return S(member: 1) }

public protocol P {
  func requirement()
  func requirementWithDefaultImpl()
}

extension P {
  public func requirementWithDefaultImpl() {}
}

public struct S {
  public var member: Int
  public static var staticMember: Int = 0

  public init(member: Int) {
    self.member = member
  }
  public func method(with argument: Int) {}
}

extension S: P {
  public func requirement() {}
}

public class C {
  public init() {}
  public func method(with argument: Int) {}
}

public enum E {
  case basicCase
  case payloadCase(_: S)
}

#if CONCURRENCY
public actor A {
  public init() {}
  public func asyncMethod() async {}
}
#endif

public struct Generic<T> {}
