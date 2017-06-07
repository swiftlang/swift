import Foundation

class MyClass : NSObject {
  var property : NSObject? = nil
  dynamic var dynamicProperty: Int { return 2 }
  func foo() {}
  func baz() {}
}

extension MyClass {
  func bar() {}
}

class MySubClass : MyClass {
  override func foo() {}
  override func bar() {}
}
