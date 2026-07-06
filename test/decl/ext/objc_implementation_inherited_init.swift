// RUN: %target-swift-frontend -emit-silgen -verify -import-objc-header %S/Inputs/objc_implementation_inherited_init.h -enable-experimental-feature ObjCImplementation -target %target-stable-abi-triple -Xcc -Wno-nullability-completeness %s
// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCImplementation

// Declaring a designated initializer suppresses inheritance of NSObject's
// 'init()', which the compiler turns into a trapping stub. Objective-C callers
// can still invoke it (e.g. '[ImplsMissingInit new]'), so it must be implemented.
@objc @implementation extension ImplsMissingInit {
  // expected-warning@-1 {{'@objc @implementation' extension does not implement initializer 'init()' inherited from its superclass; invoking it from Objective-C will trap at runtime}}
  init(value: Int32) {
    super.init()
  }
}

// Overriding 'init()' satisfies the requirement: no diagnostic.
@objc @implementation extension ImplsHasInit {
  init(value: Int32) {
    super.init()
  }

  override init() {
    super.init()
  }
}
