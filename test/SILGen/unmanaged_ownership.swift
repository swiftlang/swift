
// RUN: %target-swift-emit-silgen -parse-stdlib -module-name Swift %s | %FileCheck %s

class C {}

enum Optional<T> {
case none
case some(T)
}

precedencegroup AssignmentPrecedence {
  assignment: true
  associativity: right
}

struct Holder {
  unowned(unsafe) var value: C
}

_ = Holder(value: C())
// CHECK-LABEL:sil hidden [ossa] @$ss6HolderV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@owned C, @thin Holder.Type) -> Holder
// CHECK: bb0([[T0:%.*]] : @owned $C,
// CHECK-NEXT:   [[T1:%.*]] = ref_to_unmanaged [[T0]] : $C to $@sil_unmanaged C
// CHECK-NEXT:   destroy_value [[T0]] : $C
// CHECK-NEXT:   [[T2:%.*]] = struct $Holder ([[T1]] : $@sil_unmanaged C)
// CHECK-NEXT:   return [[T2]] : $Holder
// CHECK-NEXT: } // end sil function '$ss6HolderV5valueABs1CC_tcfC'
func set(holder holder: inout Holder) {
  holder.value = C()
}

// CHECK-LABEL: sil hidden [ossa] @$ss3set6holderys6HolderVz_tF : $@convention(thin) (@inout Holder) -> () {
// CHECK: bb0([[ADDR:%.*]] : $*Holder):
// CHECK:        [[T0:%.*]] = function_ref @$ss1CC{{[_0-9a-zA-Z]*}}fC
// CHECK:        [[C:%.*]] = apply [[T0]](
// CHECK-NEXT:   [[WRITE:%.*]] = begin_access [modify] [unknown] [[ADDR]] : $*Holder
// CHECK-NEXT:   [[T0:%.*]] = struct_element_addr [[WRITE]] : $*Holder, #Holder.value
// CHECK-NEXT:   [[T1:%.*]] = ref_to_unmanaged [[C]]
// CHECK-NEXT:   assign [[T1]] to [[T0]]
// CHECK-NEXT:   destroy_value [[C]]
// CHECK-NEXT:   end_access [[WRITE]] : $*Holder
// CHECK: } // end sil function '$ss3set6holderys6HolderVz_tF'

func get(holder holder: inout Holder) -> C {
  return holder.value
}
// CHECK-LABEL: sil hidden [ossa] @$ss3get6holders1CCs6HolderVz_tF : $@convention(thin) (@inout Holder) -> @owned C {
// CHECK: bb0([[ADDR:%.*]] : $*Holder):
// CHECK-NEXT:   debug_value_addr %0 : $*Holder, var, name "holder", argno 1 
// CHECK-NEXT:   [[READ:%.*]] = begin_access [read] [unknown] [[ADDR]] : $*Holder
// CHECK-NEXT:   [[T0:%.*]] = struct_element_addr [[READ]] : $*Holder, #Holder.value
// CHECK-NEXT:   [[T1:%.*]] = load [trivial] [[T0]] : $*@sil_unmanaged C
// CHECK-NEXT:   [[T2:%.*]] = copy_unmanaged_value [[T1]]
// CHECK-NEXT:   end_access [[READ]] : $*Holder
// CHECK-NEXT:   return [[T2]]

func project(fn fn: () -> Holder) -> C {
  return fn().value
}
// CHECK-LABEL: sil hidden [ossa] @$ss7project2fns1CCs6HolderVyXE_tF : $@convention(thin) (@noescape @callee_guaranteed () -> Holder) -> @owned C {
// CHECK: bb0([[FN:%.*]] : $@noescape @callee_guaranteed () -> Holder):
// CHECK-NEXT: debug_value
// CHECK-NEXT: [[T0:%.*]] = apply [[FN]]()
// CHECK-NEXT: [[T1:%.*]] = struct_extract [[T0]] : $Holder, #Holder.value
// CHECK-NEXT: [[T2:%.*]] = copy_unmanaged_value [[T1]]
// CHECK-NOT: destroy_value [[BORROWED_FN_COPY]]
// CHECK-NEXT: return [[T2]]
