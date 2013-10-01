// Import this class in the test.
class [objc] Bar_ImportedObjcClass {
  func bar_ImportedObjcClass_InstanceFunc1() {}
  static func bar_ImportedObjcClass_ClassFunc1() {}
  subscript(i: Bar_ImportedObjcClass) -> Int {
  get:
    return 0
  }
  var bar_ImportedObjcClass_Property1: Int
}

// Don't import this class in the test.
class [objc] Bar_NonImportedObjcClass {
  func ERROR() {}
}

