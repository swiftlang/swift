
public let global: Int = 0

public func function(with argument: Int) {}
public func genericFunc<T: P>(_ t: T) {}
@_cdecl("cdecl_func") public func cdeclFunc() {}
@_silgen_name("forward_declared_func") public func forwardDeclaredFunc()

public protocol P {
  func requirement()
  func requirementWithDefaultImpl()
}

extension P {
  public func requirementWithDefaultImpl() {}
}

public struct S {
  public static var staticMember: Int = 0
  public static func staticFunc() {}

  public var member: Int

  public init(member: Int) {
    self.member = member
  }
  public func method(with argument: Int) {}
  public func genericFunc<T: P>(_ t: T) {}
}

extension S: P {
  public func requirement() {}
}

public struct GenericS<T: P> {
  public var member: T

  public init(member: T) {
    self.member = member
  }
  public func method(with argument: T) {}
}

public class C {
  public static var staticMember: Int = 0
  public class func classFunc() {}

  public var member: Int

  public init(member: Int) {
    self.member = member
  }
  public func method(with argument: Int) {}
}

public enum E {
  case basicCase
  case payloadCase(_: S)

  public func method() {}
}
