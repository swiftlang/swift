// RUN: %target-swift-emit-ir -module-name test -enable-experimental-feature CalledAttribute -target %target-swift-6.4-abi-triple %s | %FileCheck %s --check-prefixes=CHECK,OLD
// RUN: %target-swift-emit-ir -module-name test -enable-experimental-feature CalledAttribute -target %target-future-triple %s | %FileCheck %s --check-prefixes=CHECK,NEW

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: OS=macosx || OS=maccatalyst || OS=ios || OS=tvos || OS=watchos || OS=xros

// Runtimes before Swift 6.5 don't have `swift_onceClosureDestroyedError`,
// so when the deployment target includes such a runtime, the destructor of an
// exactly-once context traps directly.

public func make(_ object: AnyObject) -> @called(exactlyOnce) () -> Void {
  return { _ = object }
}

// CHECK-LABEL: define internal swiftcc void @__swift_exactly_once_closure_destructor(ptr swiftself %0)
// OLD-NOT: swift_onceClosureDestroyedError
// OLD: call void @llvm.trap()
// NEW-NOT: llvm.trap
// NEW: call void @swift_onceClosureDestroyedError(ptr %0)
// CHECK-NEXT: unreachable
