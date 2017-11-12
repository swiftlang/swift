import Foundation

class MyClass : NSObject {
  var propertyUsedInKeyPath : NSObject? = nil
  dynamic var dynamicVarUsedInSelector : Int { return 2 }
  func overridden() {}
  func usedViaAnyObject() {}
  func unused() {}
}

extension MyClass {
  func inExtensionAndOverridden() {}
}

class MySubClass : MyClass {
  override func overridden() {}
  override func inExtensionAndOverridden() {}
}

func test(object: AnyObject, mine: MyClass) {
  _ = #selector(MyClass.overridden)
  _ = #selector(getter: MyClass.dynamicVarUsedInSelector)
  _ = #keyPath(MyClass.propertyUsedInKeyPath)
  _ = object.usedViaAnyObject?()
}

class SelfReferences : NSObject {
  var prop: Int = 2
  func foo() {
    _ = #selector(self.foo)
    _ = #keyPath(prop)
  }

  func bar() {
    _ = #selector(self.foo)
    _ = #selector(self.bar)
  }
}
