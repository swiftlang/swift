// RUN: %target-typecheck-verify-swift -Xcc -fmodule-map-file=%S/Inputs/objc_implementation_private.modulemap -enable-experimental-feature ObjCImplementation -target %target-stable-abi-triple -debug-diagnostic-names
// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCImplementation

import objc_implementation_internal

// FIXME: [availability] An implementation that is less available than the interface it implements should be diagnosed
@available(*, unavailable)
@objc @implementation extension ObjCPropertyTest {
  let prop1: Int32

  var prop2: Int32 {
    didSet {
      _ = prop2
    }
  }

  override init() {
    self.prop1 = 1
    self.prop2 = 2
    super.init()
  }

  func doSomething() {
    _ = self.prop1
    _ = self.prop2
  }
}

func takesObjCPropertyTest(_ o: ObjCPropertyTest) {
  _ = o.prop1
  _ = o.prop2
}
