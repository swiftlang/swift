
public protocol A {
  static func a()
}

public protocol B : A {
  static func b()
}

public struct S { public init() {} }

extension S : A {
  public static func a() { }
}

extension S : B {
  public static func b() { }
}
