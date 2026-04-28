// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name switch_default %s | %FileCheck %s

class Klass {}
protocol Protocol {}

enum Enum {
  case value1(Klass)
  case value2(Protocol)
}

// CHECK-LABEL: sil hidden [ossa] @$s14switch_default33testAddressOnlySubjectDefaultCaseyyAA4EnumOSgF : $@convention(thin) (@in_guaranteed Optional<Enum>) -> () {
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   [[STACK:%.*]] = alloc_stack $Optional<Enum>
// CHECK:   copy_addr [[ARG]] to [init] [[STACK]]
// CHECK:   switch_enum_addr [[STACK]] : $*Optional<Enum>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]*]], default [[DEFAULT_BB:bb[0-9]*]]
//
// CHECK: [[SOME_BB]]:
// CHECK:    [[STACK_1:%.*]] = alloc_stack $Optional<Enum>
// CHECK:    copy_addr [[STACK]] to [init] [[STACK_1]]
// CHECK:    [[TAKEN_ADDR:%.*]] = unchecked_take_enum_data_addr [[STACK_1]]
// CHECK:    switch_enum_addr [[TAKEN_ADDR]] : $*Enum, case #Enum.value2!enumelt: [[VALUE2_BB:bb[0-9]*]], default [[DEFAULT_BB_2:bb[0-9]*]]
//
// CHECK: [[VALUE2_BB]]:
// CHECK:    [[TAKEN_TAKEN_ADDR:%.*]] = unchecked_take_enum_data_addr [[TAKEN_ADDR]]
// CHECK:    destroy_addr [[TAKEN_TAKEN_ADDR]]
// CHECK:    dealloc_stack [[STACK_1]]
// CHECK:    destroy_addr [[STACK]]
// CHECK:    dealloc_stack [[STACK]]
// CHECK:    br [[EXIT_BB:bb[0-9]+]]
//
// We used to leak here!
// CHECK: [[DEFAULT_BB_2]]:
// CHECK:    destroy_addr [[TAKEN_ADDR]]
// CHECK:    dealloc_stack [[STACK_1]]
// CHECK:    br [[CONT_BB:bb[0-9]*]]
//
// CHECK: [[DEFAULT_BB]]:
// CHECK:    br [[CONT_BB]]
//
// CHECK: [[CONT_BB]]:
// CHECK:    destroy_addr [[STACK]]
// CHECK:    dealloc_stack [[STACK]]
// CHECK:    br [[EXIT_BB]]
//
// CHECK: [[EXIT_BB]]:
// CHECK-NEXT: tuple
// CHECK-NEXT: return
// } // end sil function '$s14switch_default33testAddressOnlySubjectDefaultCaseyyAA4EnumOSgF'
func testAddressOnlySubjectDefaultCase(_ e: Enum?) {
  switch (e) {
  case .value2: return
  default: return
  }
}
