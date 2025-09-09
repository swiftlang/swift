// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/test.swift
// RUN: %target-swift-frontend -enable-experimental-feature AddressableTypes -emit-ir %s | %FileCheck %t/test.swift

// REQUIRES: swift_feature_AddressableTypes

@_addressableForDependencies
struct DirectlyAFD {
    var x: Int32
}
// CHECK-LABEL: @"$s{{.*}}11DirectlyAFDVWV" =
// -- 0x0200_0000: addressable for dependencies
// CHECK-SAME: , [[WORD:i(64|32)]] 4, [[WORD]] 4, <i32 0x0200_0003>, i32 0 }

struct IndirectlyAFD {
    var directly: DirectlyAFD
}
// CHECK-LABEL: @"$s{{.*}}13IndirectlyAFDVWV" =
// CHECK-SAME: , [[WORD:i(64|32)]] 4, [[WORD]] 4, <i32 0x0200_0003>, i32 0 }
