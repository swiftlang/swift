import Foundation

public class Bar : Foo {
  @objc(method2WithValue:) public override func method2(_ value: Int) { }

  @objc(overloadedWithInt:) public func overloaded(_ x: Int) { }
  @objc(overloadedWithString:) public func overloaded(_ x: String) { }

  @objc(staticOverloadedWithInt:) public static func staticOverloaded(_ x: Int) { }
  @objc(staticOverloadedWithString:) public static func staticOverloaded(_ x: String) { }

  @objc(staticOrNonStatic:) public func staticOrNonStatic(_ x: Int) { }
  @objc(staticOrNonStatic:) public static func staticOrNonStatic(_ x: Int) { }

  @objc(theInstanceOne:) public func staticOrNonStatic2(_ x: Int) { }
  @objc(theStaticOne:) public static func staticOrNonStatic2(_ x: Int) { }
}

public class Foo {
  @objc(methodWithValue:label:) public func method(_ value: Int, label: String) { }

  @objc(method2WithValue:) public func method2(_ value: Int) { }

  @objc public func method3() { }

  @objc public var property: String = ""
}
