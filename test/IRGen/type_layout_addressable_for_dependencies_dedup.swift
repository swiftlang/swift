// RUN: %target-swift-frontend -enable-experimental-feature AddressableTypes -emit-ir %s | %FileCheck %s

// REQUIRES: swift_feature_AddressableTypes

// Two types with identical {size, EI, align, pod, bitwiseTakable} but
// differing IsAddressableForDependencies must NOT share a single private
// type_layout_* global. The Flags word inside the global encodes AFD, so
// sharing causes the second-emitted type's runtime VWT to silently inherit
// the first type's AFD bit (after the runtime-side patch that ORs each
// field's TypeLayout AFD into the aggregate VWT during instantiation).

@_addressableForDependencies
struct AFD { var x: UInt16; var y: UInt16 }

struct NotAFD { var x: UInt16; var y: UInt16 }

struct G<T> {
  var t: T
  var afd: AFD
  var notAFD: NotAFD
}

func use<T>(_ g: G<T>) {
  _ = g
}

// AFD field: alignment mask 1 (align=2) + IsAddressableForDependencies
// (0x02000000) = 0x02000001 = 33554433.
//
// CHECK-DAG: @type_layout_4_2_0_pod_afd = private constant {{.*}} {{i64|i32}} 4, {{i64|i32}} 4, i32 33554433, i32 0 }
//
// Non-AFD field: alignment mask 1 = 0x00000001 = 1.
//
// CHECK-DAG: @type_layout_4_2_0_pod = private constant {{.*}} {{i64|i32}} 4, {{i64|i32}} 4, i32 1, i32 0 }
