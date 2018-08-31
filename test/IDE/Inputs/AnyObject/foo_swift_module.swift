@objc public class Foo_TopLevelObjcClass {
  @objc public func foo_TopLevelObjcClass_InstanceFunc1() {}
  @objc public class func foo_TopLevelObjcClass_ClassFunc1() {}
  @objc public subscript(i: Int32) -> Int {
    get {
      return 0
    }
  }
  @objc public var foo_TopLevelObjcClass_Property1: Int = 0

  internal func foo_TopLevelObjcClass_internalFunc_ERROR() {}
  private func foo_TopLevelObjcClass_privateFunc_ERROR() {}
}

public class Foo_TopLevelClass {
  @objc public func foo_TopLevelClass_ObjcInstanceFunc1() {}
  @objc public class func foo_TopLevelClass_ObjcClassFunc1() {}
  @objc public subscript (i: Int64) -> Int {
    get {
      return 0
    }
  }
  @objc public var foo_TopLevelClass_ObjcProperty1: Int = 0
}

@objc public protocol Foo_TopLevelObjcProtocol {
  @objc func foo_TopLevelObjcProtocol_InstanceFunc1()
  @objc static func foo_TopLevelObjcProtocol_ClassFunc1()
  @objc subscript(i: Foo_TopLevelObjcProtocol) -> Int { get set }
  @objc var foo_TopLevelObjcProtocol_Property1: Int { get }
}

public class Foo_ContainerForNestedClass1 {
  public class Foo_Nested1 {
    @objc public func foo_Nested1_ObjcInstanceFunc1() {}
    @objc public class func foo_Nested1_ObjcClassFunc1() {}
    @objc public var foo_Nested1_Property1: Int = 0
    public func ERROR() {}
  }
  public func ERROR() {}
}

public struct Foo_ContainerForNestedClass2 {
  public class Foo_Nested2 {
    @objc public func foo_Nested2_ObjcInstanceFunc1() {}
    @objc public class func foo_Nested2_ObjcClassFunc1() {}
    @objc public var foo_Nested2_Property1: Int = 0
    public func ERROR() {}
  }
  public func ERROR() {}
}


