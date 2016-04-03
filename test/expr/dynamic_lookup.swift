// RUN: %target-parse-verify-swift

@objc class HasStaticProperties {
  class var staticVar1: Int { return 4 }
}

func testStaticProperty(_ classObj: AnyObject.Type) {
  _ = classObj.staticVar1
}
