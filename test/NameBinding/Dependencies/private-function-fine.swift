// RUN: %target-swift-frontend -enable-fine-grained-dependencies -emit-silgen -primary-file %s %S/Inputs/InterestingType.swift -DOLD -emit-reference-dependencies-path %t.swiftdeps -module-name main | %FileCheck %s -check-prefix=CHECK-OLD
// RUN: awk '/kind:/ {k = $2}; /aspect:/ {a = $2}; /context:/ {c = $2}; /name/ {n = $2}; /sequenceNumber/ {s = $2}; /isProvides:/ {print k, a, c, n, $2}' <%t.swiftdeps   | sort  >%t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=CHECK-DEPS %s < %t-processed.swiftdeps

// RUN: %target-swift-frontend -enable-fine-grained-dependencies -emit-silgen -primary-file %s %S/Inputs/InterestingType.swift -DNEW -emit-reference-dependencies-path %t.swiftdeps -module-name main | %FileCheck %s -check-prefix=CHECK-NEW
// RUN: awk '/kind:/ {k = $2}; /aspect:/ {a = $2}; /context:/ {c = $2}; /name/ {n = $2}; /sequenceNumber/ {s = $2}; /isProvides:/ {print k, a, c, n, $2}' <%t.swiftdeps   | sort  >%t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=CHECK-DEPS %s < %t-processed.swiftdeps

private func testParamType(_: InterestingType) {}

// CHECK-OLD: sil_global hidden @$s4main1x{{[^ ]+}} : ${{(@[a-zA-Z_]+ )?}}(Int) -> ()
// CHECK-NEW: sil_global hidden @$s4main1x{{[^ ]+}} : ${{(@[a-zA-Z_]+ )?}}(Double) -> ()
internal var x = testParamType

// CHECK-DEPS: topLevel interface  '' InterestingType false
