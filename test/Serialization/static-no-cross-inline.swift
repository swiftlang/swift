// Regression test for #88199: a function whose body references symbols owned
// by a foreign `-static` module must not have its body serialized for
// cross-module inlining. Otherwise, when a non-static client inlines such a
// body, the resulting code emits a direct reference to the static module's
// symbol — which may not be exported by whichever shared library the client
// links against, and the link will fail.
//
// The test exercises the 3-module chain from the issue:
//
//   StaticC (`-static`)  →  defines `f3`
//   DynamicB             →  defines `f2` whose body calls `f3`
//   Main                 →  imports DynamicB and calls `f2`
//
// We expect DynamicB's serialized SIL for `f2` to be a declaration only
// (no body) when StaticC is built with `-static`. As a control, when
// StaticC is built without `-static`, `f2`'s body should be serialized
// normally.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// --- Static case: StaticC built with -static. f2's body must be dropped.
// RUN: %target-swift-frontend -emit-module -static -parse-as-library \
// RUN:   -module-name StaticC %t/StaticC.swift \
// RUN:   -emit-module-path %t/StaticC.swiftmodule
// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name DynamicB -I %t %t/DynamicB.swift \
// RUN:   -emit-module-path %t/DynamicB.swiftmodule
// RUN: %target-sil-opt -enable-sil-verify-all -I %t %t/DynamicB.swiftmodule \
// RUN:   -emit-sorted-sil -o - | %FileCheck %s --check-prefix=CHECK-STATIC

// --- Control case: StaticC built without -static. f2's body must be kept.
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name StaticC %t/StaticC.swift \
// RUN:   -emit-module-path %t/StaticC.swiftmodule
// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name DynamicB -I %t %t/DynamicB.swift \
// RUN:   -emit-module-path %t/DynamicB.swiftmodule
// RUN: %target-sil-opt -enable-sil-verify-all -I %t %t/DynamicB.swiftmodule \
// RUN:   -emit-sorted-sil -o - | %FileCheck %s --check-prefix=CHECK-DYNAMIC

// In the static case, f2's body is not serialized — the `sil` line for
// f2 ends without a `{` opening a body.
// CHECK-STATIC: sil {{.*}}@$s8DynamicB2f2SiyF{{.*}} -> Int{{[[:space:]]*$}}

// In the control case, f2's body is serialized: the `sil` line for f2
// ends with `{` and a `function_ref` to f3 appears inside.
// CHECK-DYNAMIC: sil {{.*}}@$s8DynamicB2f2SiyF{{.*}} -> Int {
// CHECK-DYNAMIC: function_ref @$s7StaticC2f3SiyF

//--- StaticC.swift

@inlinable
public func f3() -> Int {
  return 42
}

//--- DynamicB.swift

import StaticC

@inlinable
public func f2() -> Int {
  return f3() &+ 1
}
