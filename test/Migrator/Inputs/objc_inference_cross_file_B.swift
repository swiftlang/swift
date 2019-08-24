public func test(object: AnyObject, mine: MyClass) {
  _ = #selector(MyClass.methodUsedInSelector)
  _ = #selector(getter: MyClass.dynamicVarUsedInSelector)
  _ = #keyPath(MyClass.propertyUsedInKeyPath)
}
