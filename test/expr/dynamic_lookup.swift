// RUN: %target-typecheck-verify-swift -enable-objc-interop

@objc class HasStaticProperties {
  @objc class var staticVar1: Int { return 4 }
}

func testStaticProperty(classObj: AnyObject.Type) {
  _ = classObj.staticVar1
}
