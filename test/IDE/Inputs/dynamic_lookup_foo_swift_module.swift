class [objc] Foo_TopLevelObjcClass {
  func foo_TopLevelObjcClass_InstanceFunc1() {}
  static func foo_TopLevelObjcClass_ClassFunc1() {}
}

class Foo_TopLevelClass {
  func [objc] foo_TopLevelClass_ObjcInstanceFunc1() {}
  static func [objc] foo_TopLevelClass_ObjcClassFunc1() {}
}

class Foo_ContainerForNestedClass1 {
  class Foo_Nested1 {
    func [objc] foo_Nested1_ObjcInstanceFunc1() {}
    static func [objc] foo_Nested1_ObjcClassFunc1() {}
    func ERROR() {}
  }
  func ERROR() {}
}

struct Foo_ContainerForNestedClass2 {
  class Foo_Nested2 {
    func [objc] foo_Nested2_ObjcInstanceFunc1() {}
    static func [objc] foo_Nested2_ObjcClassFunc1() {}
    func ERROR() {}
  }
  func ERROR() {}
}

