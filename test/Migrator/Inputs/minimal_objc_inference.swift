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
