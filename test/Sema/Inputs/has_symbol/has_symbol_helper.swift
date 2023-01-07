public let global: Int = 0

public func noArgFunc() {}
public func function(with argument: Int) {}
public func throwingFunc() throws {}
public func ambiguousFunc() {}
public func ambiguousFunc() -> Int { return 0 }
public func genericFunc<T: P>(_ t: T) {}

public protocol P {
  func requirement()
  func requirementWithDefaultImpl()
}

extension P {
  public func requirementWithDefaultImpl() {}
}

public protocol PAT {
  associatedtype A
  func requirement() -> A
  func requirementWithDefaultImpl()
}

extension PAT {
  public func requirementWithDefaultImpl() {}
}

public struct S {
  public static var staticMember: Int = 0
  public static func staticFunc() {}

  public var member: Int

  public init(member: Int) {
    self.member = member
  }
  public func noArgsMethod() {}
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
  public func noArgsMethod() {}
  public func method(with argument: T) {}
}

public class C {
  public static var staticMember: Int = 0
  public class func classFunc() {}

  public var member: Int

  public init(member: Int) {
    self.member = member
  }
  public func noArgsMethod() {}
  public func method(with argument: Int) {}
}

extension C: P {
  public func requirement() {}
}

public enum E {
  case basicCase
  case payloadCase(_: S)

  public func method() {}
}
