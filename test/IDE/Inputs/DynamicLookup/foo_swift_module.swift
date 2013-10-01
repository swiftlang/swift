class [objc] Foo_TopLevelObjcClass {
  func foo_TopLevelObjcClass_InstanceFunc1() {}
  static func foo_TopLevelObjcClass_ClassFunc1() {}
  subscript(i: Int32) -> Int {
  get:
    return 0
  }
  var foo_TopLevelObjcClass_Property1: Int
}

class Foo_TopLevelClass {
  func [objc] foo_TopLevelClass_ObjcInstanceFunc1() {}
  static func [objc] foo_TopLevelClass_ObjcClassFunc1() {}
  subscript [objc] (i: Int64) -> Int {
  get:
    return 0
  }
  var [objc] foo_TopLevelClass_ObjcProperty1: Int
}

protocol [objc,class_protocol] Foo_TopLevelObjcProtocol {
  func foo_TopLevelObjcProtocol_InstanceFunc1()
  static func foo_TopLevelObjcProtocol_ClassFunc1()
  subscript(i: Foo_TopLevelObjcProtocol) -> Int
  var foo_TopLevelObjcProtocol_Property1: Int
}

class Foo_ContainerForNestedClass1 {
  class Foo_Nested1 {
    func [objc] foo_Nested1_ObjcInstanceFunc1() {}
    static func [objc] foo_Nested1_ObjcClassFunc1() {}
    var [objc] foo_Nested1_Property1: Int
    func ERROR() {}
  }
  func ERROR() {}
}

struct Foo_ContainerForNestedClass2 {
  class Foo_Nested2 {
    func [objc] foo_Nested2_ObjcInstanceFunc1() {}
    static func [objc] foo_Nested2_ObjcClassFunc1() {}
    var [objc] foo_Nested2_Property1: Int
    func ERROR() {}
  }
  func ERROR() {}
}

