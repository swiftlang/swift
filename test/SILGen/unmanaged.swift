
// RUN: %target-swift-emit-sil -module-name unmanaged %s | %FileCheck %s

class C {}

struct Holder {
  unowned(unsafe) var value: C
}
_ = Holder(value: C())
// CHECK-LABEL:sil hidden @$s9unmanaged6HolderV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@owned C, @thin Holder.Type) -> Holder
// CHECK: bb0([[T0:%.*]] : $C,
// CHECK-NEXT:   [[T1:%.*]] = ref_to_unmanaged [[T0]] : $C to $@sil_unmanaged C
// CHECK-NEXT:   strong_release [[T0]] : $C
// CHECK-NEXT:   [[T2:%.*]] = struct $Holder ([[T1]] : $@sil_unmanaged C)
// CHECK-NEXT:   return [[T2]] : $Holder

func set(holder holder: inout Holder) {
  holder.value = C()
}
// CHECK-LABEL:sil hidden @$s9unmanaged3set6holderyAA6HolderVz_tF : $@convention(thin) (@inout Holder) -> ()
// CHECK: bb0([[ADDR:%.*]] : $*Holder):
// CHECK:        [[T0:%.*]] = function_ref @$s9unmanaged1CC{{[_0-9a-zA-Z]*}}fC
// CHECK:        [[C:%.*]] = apply [[T0]](
// CHECK-NEXT:   [[WRITE:%.*]] = begin_access [modify] [static] [[ADDR]] : $*Holder
// CHECK-NEXT:   [[T0:%.*]] = struct_element_addr [[WRITE]] : $*Holder, #Holder.value
// CHECK-NEXT:   [[T1:%.*]] = ref_to_unmanaged [[C]]
// CHECK-NEXT:   store [[T1]] to [[T0]]
// CHECK-NEXT:   strong_release [[C]]
// CHECK-NEXT:   end_access [[WRITE]] : $*Holder
// CHECK-NEXT:   tuple ()
// CHECK-NEXT:   return

func get(holder holder: inout Holder) -> C {
  return holder.value
}
// CHECK-LABEL:sil hidden @$s9unmanaged3get6holderAA1CCAA6HolderVz_tF : $@convention(thin) (@inout Holder) -> @owned C
// CHECK: bb0([[ADDR:%.*]] : $*Holder):
// CHECK-NEXT:   debug_value_addr %0 : $*Holder, var, name "holder", argno 1 
// CHECK-NEXT:   [[READ:%.*]] = begin_access [read] [static] [[ADDR]] : $*Holder
// CHECK-NEXT:   [[T0:%.*]] = struct_element_addr [[READ]] : $*Holder, #Holder.value
// CHECK-NEXT:   [[T1:%.*]] = load [[T0]] : $*@sil_unmanaged C
// CHECK-NEXT:   [[T2:%.*]] = copy_unmanaged_value [[T1]]
// CHECK-NEXT:   end_access [[READ]] : $*Holder
// CHECK-NEXT:   return [[T2]]

func project(fn fn: () -> Holder) -> C {
  return fn().value
}
// CHECK-LABEL: sil hidden @$s9unmanaged7project2fnAA1CCAA6HolderVyXE_tF : $@convention(thin) (@noescape @callee_guaranteed () -> Holder) -> @owned C
// CHECK: bb0([[FN:%.*]] : $@noescape @callee_guaranteed () -> Holder):
// CHECK-NEXT: debug_value
// CHECK-NEXT: [[T0:%.*]] = apply [[FN]]()
// CHECK-NEXT: [[T1:%.*]] = struct_extract [[T0]] : $Holder, #Holder.value
// CHECK-NEXT: [[T2:%.*]] = copy_unmanaged_value [[T1]]
// CHECK-NEXT: return [[T2]]
