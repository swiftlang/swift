// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// -enable-implicit-dynamic leaves direct members alone, so they are not
// replaceable; a written 'dynamic' is still an error.

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ObjCDirect -enable-implicit-dynamic
// RUN: %target-swift-emit-silgen -enable-experimental-feature ObjCDirect \
// RUN:   -enable-implicit-dynamic -D NO_ERRORS %s | %FileCheck %s

import Foundation

class ImplicitlyDynamic: NSObject {
  @objcDirect final func method() {}
  @objcDirect var computed: Int { 0 }
  @objcDirect subscript(i: Int) -> Int { i }

  // Control: still made dynamic.
  func plain() {}
}

#if !NO_ERRORS
class ExplicitlyDynamic: NSObject {
  @objcDirect dynamic func method() {}
  // expected-error@-1 {{'@objcDirect' cannot be applied to a 'dynamic' instance method; 'dynamic' requires dispatch through the Objective-C runtime}}
}
#endif

// CHECK-DAG: sil hidden [dynamically_replacable] {{.*}}@$s{{.*}}17ImplicitlyDynamicC5plainyyF :
// CHECK-DAG: sil hidden [ossa] @$s{{.*}}17ImplicitlyDynamicC6methodyyF :
// CHECK-DAG: sil hidden [ossa] @$s{{.*}}17ImplicitlyDynamicC8computedSivg :
// CHECK-DAG: sil hidden [ossa] @$s{{.*}}17ImplicitlyDynamicCyS2icig :
