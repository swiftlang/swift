// RUN: %target-swift-emit-silgen -module-name switch %s > %t.sil
// RUN: %target-sil-opt %t.sil -sil-combine -enable-sil-verify-all | %FileCheck %s

protocol P { }

enum ProtocolAndTrivial {
    case a(P)
    case b(Int)
}

// This test is mainly checking the verifier doesn't error but, just in case,
// there's also a filecheck test.
// CHECK-LABEL: @$s6switch4testyyAA18ProtocolAndTrivialO_ADtF
// CHECK: [[A1:%[0-9]+]] = alloc_stack $ProtocolAndTrivial
// CHECK: [[A2:%[0-9]+]] = alloc_stack $ProtocolAndTrivial
// CHECK: [[C1:%[0-9]+]] = alloc_stack $(ProtocolAndTrivial, ProtocolAndTrivial)

// Make sure we destroy both arg temp allocs.
// CHECK: copy_addr [take] [[A1]]
// CHECK: copy_addr [take] [[A2]]

// CHECK: [[CA:%[0-9]+]] = tuple_element_addr [[C1]] : $*(ProtocolAndTrivial, ProtocolAndTrivial), 0
// CHECK: [[CB:%[0-9]+]] = tuple_element_addr [[C1]] : $*(ProtocolAndTrivial, ProtocolAndTrivial), 1

// CHECK: switch_enum_addr [[CA]] : $*ProtocolAndTrivial, case #ProtocolAndTrivial.a!enumelt: [[BBA:bb[0-9]+]], case #ProtocolAndTrivial.b!enumelt: [[BBB:bb[0-9]+]]

// We're only testing the trivial case.
// CHECK: [[BBB]]:
// CHECK: [[TMP:%[0-9]+]] = alloc_stack $ProtocolAndTrivial
// CHECK: switch_enum_addr [[CB]] : $*ProtocolAndTrivial, case #ProtocolAndTrivial.b!enumelt: [[BOTH_B:bb[0-9]+]], default [[DEFAULT:bb[0-9]+]]

// Make sure that we destroy everything.
// CHECK: [[BOTH_B]]:
// CHECK: destroy_addr [[CB]]
// CHECK: destroy_addr [[TMP]]
// CHECK: destroy_addr [[CA]]

// CHECK-LABEL: end sil function '$s6switch4testyyAA18ProtocolAndTrivialO_ADtF'
func test(_ a : ProtocolAndTrivial, _ b : ProtocolAndTrivial) {
    switch ((a, b)) {
    case (.a(_), .a(_)): _ = ()
    case (.b(_), .b(_)): _ = ()
    default: _ = ()
    }
}
