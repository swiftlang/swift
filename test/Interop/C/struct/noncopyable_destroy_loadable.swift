// RUN: %target-swift-frontend -emit-ir -O -I %S/Inputs %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -O -I %S/Inputs %s -cxx-interoperability-mode=default | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -Onone -I %S/Inputs %s | %FileCheck %s --check-prefix=NOOPT
// RUN: %target-swift-frontend -emit-ir -Onone -I %S/Inputs %s -cxx-interoperability-mode=default | %FileCheck %s --check-prefix=NOOPT

// A loadable imported record must invoke its synthesized deinit when
// consumed, even if all its fields are trivially destroyable.
// This must hold for both address-based and value-based destruction.

import NoncopyableStructs

// CHECK-LABEL: define {{.*}}swiftcc void @"{{.*}}consume{{.*}}"(
// CHECK: {{(call|invoke)}} void @{{.*}}freeNonCopyableWithDeinit
// CHECK: ret void
// NOOPT-LABEL: define {{.*}}swiftcc void @"{{.*}}consume{{.*}}"(
// NOOPT: call swiftcc void @"$sSo21NonCopyableWithDeinitVfD"
// NOOPT: ret void
@inline(never)
public func consume(_ owner: consuming NonCopyableWithDeinit) {}

// Materialize the imported deinit in this module as well as checking that
// destroying a freshly initialized value preserves its side effects.
// CHECK-LABEL: define {{.*}}swiftcc void @"{{.*}}create{{.*}}"(
// CHECK: {{(call|invoke)}} void @{{.*}}freeNonCopyableWithDeinit
// CHECK: ret void
// NOOPT-LABEL: define {{.*}}swiftcc void @"{{.*}}create{{.*}}"(
// NOOPT: call swiftcc void @"$sSo21NonCopyableWithDeinitVfD"
// NOOPT: ret void
public func create() { let _ = NonCopyableWithDeinit() }
