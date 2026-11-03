// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Mod.swiftinterface) %s \
// RUN:   -module-name Mod -enable-experimental-feature ObjCDirect

// The interface records the feature and not the Clang flag.
// RUN: %FileCheck %s --input-file %t/Mod.swiftinterface \
// RUN:   --implicit-check-not='fobjc-direct-precondition-thunk'

// Rebuilt with no flags: the interface itself has to enable the feature.
// RUN: %target-swift-typecheck-module-from-interface(%t/Mod.swiftinterface) \
// RUN:   -module-name Mod

// CHECK: swift-module-flags:{{.*}}-enable-experimental-feature ObjCDirect
// CHECK: #if compiler(>=5.3) && $ObjCDirect
// CHECK-NEXT: @objcDirect{{.*}}func lookup()
// CHECK-NEXT: #endif
// CHECK: #if compiler(>=5.3) && $ObjCDirect
// CHECK-NEXT: @objcDirect{{.*}}var count:
// CHECK: #if compiler(>=5.3) && $ObjCDirect
// CHECK-NEXT: @objcDirect{{.*}}subscript(

// CHECK: #if compiler(>=5.3) && $ObjCDirect
// CHECK-NEXT: @objc @objcDirect convenience public init(twice: Swift::Int)
// CHECK-NEXT: #endif
// CHECK: #if compiler(>=5.3) && $ObjCDirect
// CHECK-NEXT: @objc @objcDirect public init(value: Swift::Int)
// CHECK-NEXT: #endif

import Foundation

@objc public class Cache: NSObject {
  @objcDirect public final func lookup() {}
  @objcDirect public var count: Int = 0
  @objcDirect public subscript(i: Int) -> Int { i }
  public init(value: Int) { super.init() }
  @objcDirect public convenience init(twice: Int) { self.init(value: twice * 2) }
}

public final class Sealed: NSObject {
  @objcDirect public init(value: Int) { super.init() }
}
