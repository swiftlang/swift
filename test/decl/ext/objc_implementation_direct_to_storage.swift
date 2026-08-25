// RUN: %target-typecheck-verify-swift -Xcc -fmodule-map-file=%S/Inputs/objc_implementation_private.modulemap -target %target-stable-abi-triple
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import objc_implementation_internal

@available(macOS, unavailable)
@objc @implementation extension ObjCPropertyTest {
  // expected-warning@-1 {{'@objc @implementation' extension cannot implement class 'ObjCPropertyTest' because it is unavailable in macOS; this will be an error in a future Swift language mode}}
  // expected-note@-3 {{extension of 'ObjCPropertyTest' has been explicitly marked unavailable here}}
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
