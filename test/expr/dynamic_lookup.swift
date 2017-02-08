// RUN: %target-typecheck-verify-swift

@objc class HasStaticProperties {
  class var staticVar1: Int { return 4 }
}

func testStaticProperty(classObj: AnyObject.Type) {
  _ = classObj.staticVar1
}
