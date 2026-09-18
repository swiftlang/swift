// RUN: %target-swift-frontend -primary-file %s -module-name main -O -g -target arm64-apple-ios12.5.8 -emit-sil | %FileCheck %s

// REQUIRES: PTRSIZE=64

// This test expects the stdlib to be in its properly optimized form.
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// REQUIRES: OS=ios
// REQUIRES: CPU=arm64

// Verify that after RetainSinking runs, the retain of the __EmptyArrayStorage
// is _above_ the call to $sSD17dictionaryLiteralSDyxq_Gx_q_td_tcfCSS_SSTg5
// which consumes the reference.
// rdar://114699006

// CHECK-LABEL: sil {{.*}}@$s4main1CCACycfc : {{.*}} {
// CHECK:         [[REF:%[^,]+]] = raw_pointer_to_ref {{%[0-9]+}} to $__EmptyArrayStorage
// CHECK:         [[BRIDGE_OBJECT:%[^,]+]] = unchecked_ref_cast [[REF]] to $Builtin.BridgeObject
// CHECK:         [[BRIDGE_STORAGE:%[^,]+]] = struct $_BridgeStorage<__ContiguousArrayStorageBase> ([[BRIDGE_OBJECT]])
// CHECK:         [[ARRAY_BUFFER:%[^,]+]] = struct $_ArrayBuffer<(String, String)> ([[BRIDGE_STORAGE]])
// CHECK:         [[ARRAY:%[^,]+]] = struct $Array<(String, String)> ([[ARRAY_BUFFER]])
// CHECK:         [[DICTIONARY_INIT:%[^,]+]] = function_ref @$sSD17dictionaryLiteralSDyxq_Gx_q_td_tcfCSS_SST{{[t0-9]*}}g5
// CHECK-NEXT:    strong_retain [[REF]]
// CHECK-NEXT:    apply [[DICTIONARY_INIT]]([[ARRAY]]
// CHECK-LABEL: } // end sil function '$s4main1CCACycfc'

class C {
    var d: [String : String] = [:]
}
