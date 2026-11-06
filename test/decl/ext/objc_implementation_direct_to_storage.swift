// RUN: %target-typecheck-verify-swift -Xcc -fmodule-map-file=%S/Inputs/objc_implementation_private.modulemap -target %target-stable-abi-triple -verify-ignore-unrelated -swift-version 6 -verify-additional-prefix swift6-
// RUN: %if swift_future %{ %target-typecheck-verify-swift -swift-version future -Xcc -fmodule-map-file=%S/Inputs/objc_implementation_private.modulemap -target %target-stable-abi-triple -verify-ignore-unrelated -verify-additional-prefix future- %}
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import objc_implementation_internal

@available(macOS, unavailable)
@objc @implementation extension ObjCPropertyTest {
  // expected-swift6-warning@-1 {{'@objc @implementation' extension cannot implement class 'ObjCPropertyTest' because it is unavailable in macOS; this will be an error in a future Swift language mode}}
  // expected-note@-3 {{extension of 'ObjCPropertyTest' has been explicitly marked unavailable here}}
  // expected-future-error@-3 {{'@objc @implementation' extension cannot implement class 'ObjCPropertyTest' because it is unavailable in macOS}}
  let prop1: Int32

  var prop2: Int32 {
    didSet {
      _ = prop2
    }
  }

  override init() {
    // expected-swift6-warning@-1 {{cannot override 'init' with a declaration that is marked unavailable; this will be an error in a future Swift language mode}}
    // expected-future-error@-2 {{cannot override 'init' with a declaration that is marked unavailable}}
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
