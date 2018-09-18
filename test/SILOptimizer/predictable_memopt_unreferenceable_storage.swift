// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %s | %FileCheck %s

import ctypes
struct S {
    let a: Int
    let b: Int
    let c: Int
}
struct T {
    let v: StructWithBitfields
    let s: S
    init(v: StructWithBitfields, s: S) {
        self.v = v
        self.s = s
    }
}
// CHECK-LABEL: sil hidden @$s42predictable_memopt_unreferenceable_storage1TV1v1sACSo19StructWithBitfieldsV_AA1SVtcfC
// CHECK:       bb0(%0 : $StructWithBitfields, %1 : $S, %2 : $@thin T.Type):
// CHECK:         [[RESULT:%.*]] = struct $T (%0 : $StructWithBitfields, %1 : $S)
// CHECK:         return [[RESULT]]
