// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// @objcDirect is printed into the .swiftinterface, so the interface has to
// carry enough information to be rebuilt from. That is the reason the
// attribute is gated on a Swift feature rather than directly on Clang's
// -fobjc-direct-precondition-thunk: -Xcc is not a ModuleInterfaceOption and so
// never reaches swift-module-flags, whereas -enable-experimental-feature does,
// and enabling the feature re-derives the Clang flag. Gating on the Clang
// option instead makes the second RUN line below fail -- the interface would
// print an attribute that the rebuild then rejects.

// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Mod.swiftinterface) %s \
// RUN:   -module-name Mod -enable-experimental-feature ObjCDirect

// The interface records the feature, and deliberately does not record the
// Clang flag -- checked by the implicit-check-not.
// RUN: %FileCheck %s --input-file %t/Mod.swiftinterface \
// RUN:   --implicit-check-not='fobjc-direct-precondition-thunk'

// Rebuilding mentions neither the feature nor the Clang flag on the command
// line: everything needed has to come from the interface itself.
// RUN: %target-swift-typecheck-module-from-interface(%t/Mod.swiftinterface) \
// RUN:   -module-name Mod

// CHECK: swift-module-flags:{{.*}}-enable-experimental-feature ObjCDirect
// CHECK: @objcDirect{{.*}}func lookup()

import Foundation

@objc public class Cache: NSObject {
  @objcDirect public final func lookup() {}
}
