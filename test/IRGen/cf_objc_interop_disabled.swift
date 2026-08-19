// RUN: %target-swift-frontend -emit-ir -module-name main -disable-objc-interop -sdk %S/Inputs %s | %FileCheck %s --implicit-check-not=llvm.objc.retain --implicit-check-not=llvm.objc.release

// A Core Foundation type imports as a class on every platform.
// With interop disabled it must use *native* reference counting, not ObjC,
// since in swift-corelibs-foundation a CF object is a Swift heap object, and
// CFRetain/CFRelease call straight through to swift_retain/swift_release.

import CoreCooling

public func passthrough(_ fridge: CCRefrigerator) -> CCRefrigerator {
  return fridge
}

// CHECK-LABEL: define {{.*}}@"$s4main11passthrough{{.*}}"
// CHECK:         swift_retain
