@objc class Foo_TopLevelObjcClass {
  func foo_TopLevelObjcClass_InstanceFunc1() {}
  class func foo_TopLevelObjcClass_ClassFunc1() {}
  subscript(i: Int32) -> Int {
    get {
      return 0
    }
  }
  var foo_TopLevelObjcClass_Property1: Int = 0
}

class Foo_TopLevelClass {
  @objc func foo_TopLevelClass_ObjcInstanceFunc1() {}
  @objc class func foo_TopLevelClass_ObjcClassFunc1() {}
  @objc subscript (i: Int64) -> Int {
    get {
      return 0
    }
  }
  @objc var foo_TopLevelClass_ObjcProperty1: Int = 0
}

@class_protocol @objc protocol Foo_TopLevelObjcProtocol {
  func foo_TopLevelObjcProtocol_InstanceFunc1()
  class func foo_TopLevelObjcProtocol_ClassFunc1()
  subscript(i: Foo_TopLevelObjcProtocol) -> Int { get set }
  var foo_TopLevelObjcProtocol_Property1: Int { get }
}

class Foo_ContainerForNestedClass1 {
  class Foo_Nested1 {
    @objc func foo_Nested1_ObjcInstanceFunc1() {}
    @objc class func foo_Nested1_ObjcClassFunc1() {}
    @objc var foo_Nested1_Property1: Int = 0
    func ERROR() {}
  }
  func ERROR() {}
}

struct Foo_ContainerForNestedClass2 {
  class Foo_Nested2 {
    @objc func foo_Nested2_ObjcInstanceFunc1() {}
    @objc class func foo_Nested2_ObjcClassFunc1() {}
    @objc var foo_Nested2_Property1: Int = 0
    func ERROR() {}
  }
  func ERROR() {}
}

