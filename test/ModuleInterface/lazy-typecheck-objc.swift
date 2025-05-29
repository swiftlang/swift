// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/baseline)
// RUN: %empty-directory(%t/lazy)

// RUN: %target-swift-emit-module-interface(%t/baseline/Test.swiftinterface) -module-name Test %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-typecheck-module-from-interface(%t/baseline/Test.swiftinterface) -module-name Test

// RUN: %target-swift-emit-module-interface(%t/lazy/Test.swiftinterface) -module-name Test %s -disable-objc-attr-requires-foundation-module -experimental-lazy-typecheck
// RUN: %target-swift-typecheck-module-from-interface(%t/lazy/Test.swiftinterface) -module-name Test
// RUN: diff -u %t/baseline/Test.swiftinterface %t/lazy/Test.swiftinterface

// REQUIRES: objc_interop

@objc open class ObjCClass {
  public var publicVar: Int = 0
  @NSManaged public var managedVar: Int
  open func method() {}
}

public final class FinalClass {
  @objc public dynamic var publicDynamicVar: Int {
    get { return 0 }
    set {}
  }
}
