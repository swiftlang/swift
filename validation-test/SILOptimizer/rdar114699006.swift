// RUN: %target-swift-frontend \
// RUN:     -primary-file %s   \
// RUN:     -module-name main  \
// RUN:     -O                 \
// RUN:     -g                 \
// RUN:     -target x86_64-apple-macos10.13 \
// RUN:     -emit-ir           \
// RUN:     -o /dev/null       \
// RUN:     -Xllvm -sil-print-types \
// RUN:     -Xllvm -sil-print-function='$s4main1CCACycfc' \
// RUN:     2>&1 | %FileCheck %s

// REQUIRES: PTRSIZE=64

// This test expects the stdlib to be in its properly optimized form.
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// No need to run this test that has a hard-coded target of macos10.13 on other
// platforms.
// REQUIRES: OS=macosx
// REQUIRES: STDLIB_VARIANT=macosx-x86_64

// Verify that after RetainSinking runs, the retain of the __EmptyArrayStorage
// is _above_ the call to $sSD17dictionaryLiteralSDyxq_Gx_q_td_tcfCSS_SSTg5
// which consumes the reference.
// rdar://114699006

// CHECK-LABEL: *** SIL function after {{.*}} RetainSinking (retain-sinking)
// CHECK-LABEL: sil {{.*}}@$s4main1CCACycfc : {{.*}} {
// CHECK:         [[REF:%[^,]+]] = raw_pointer_to_ref {{%[^,]+}} : $Builtin.RawPointer to $__EmptyArrayStorage
// CHECK:         [[BRIDGE_OBJECT:%[^,]+]] = unchecked_ref_cast [[REF]] : $__EmptyArrayStorage to $Builtin.BridgeObject
// CHECK:         [[BRIDGE_STORAGE:%[^,]+]] = struct $_BridgeStorage<__ContiguousArrayStorageBase> ([[BRIDGE_OBJECT]] :
// CHECK:         [[ARRAY_BUFFER:%[^,]+]] = struct $_ArrayBuffer<(String, String)> ([[BRIDGE_STORAGE]] :
// CHECK:         [[ARRAY:%[^,]+]] = struct $Array<(String, String)> ([[ARRAY_BUFFER]] :
// CHECK:         [[DICTIONARY_INIT:%[^,]+]] = function_ref @$sSD17dictionaryLiteralSDyxq_Gx_q_td_tcfCSS_SST{{[t0-9]*}}g5
// CHECK-NEXT:    strong_retain [[REF]] : $__EmptyArrayStorage
// CHECK-NEXT:    apply [[DICTIONARY_INIT]]([[ARRAY]]
// CHECK-LABEL: } // end sil function '$s4main1CCACycfc'

class C {
    var d: [String : String] = [:]
}
