// RUN: %target-parse-verify-swift

@objc class HasStaticProperties {
  class var staticVar1: Int { return 4 }
}

func testStaticProperty(classObj: AnyObject.Type) {
  var _ = classObj.staticVar1
}
