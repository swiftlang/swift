// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

class C {}

struct Holder {
  unowned(unsafe) var value: C
}
_ = Holder(value: C())
// CHECK-LABEL:sil hidden @_T09unmanaged6HolderV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@owned C, @thin Holder.Type) -> Holder
// CHECK: bb0([[T0:%.*]] : $C,
// CHECK-NEXT:   [[T1:%.*]] = ref_to_unmanaged [[T0]] : $C to $@sil_unmanaged C
// CHECK-NEXT:   strong_release [[T0]] : $C
// CHECK-NEXT:   [[T2:%.*]] = struct $Holder ([[T1]] : $@sil_unmanaged C)
// CHECK-NEXT:   return [[T2]] : $Holder

func set(holder holder: inout Holder) {
  holder.value = C()
}
// CHECK-LABEL:sil hidden @_T09unmanaged3setyAA6HolderVz6holder_tF : $@convention(thin) (@inout Holder) -> ()
// CHECK: bb0([[ADDR:%.*]] : $*Holder):
// CHECK:        [[T0:%.*]] = function_ref @_T09unmanaged1CC{{[_0-9a-zA-Z]*}}fC
// CHECK:        [[C:%.*]] = apply [[T0]](
// CHECK-NEXT:   [[T0:%.*]] = struct_element_addr [[ADDR]] : $*Holder, #Holder.value
// CHECK-NEXT:   [[T1:%.*]] = ref_to_unmanaged [[C]]
// CHECK-NEXT:   store [[T1]] to [[T0]]
// CHECK-NEXT:   strong_release [[C]]
// CHECK-NEXT:   tuple ()
// CHECK-NEXT:   return

func get(holder holder: inout Holder) -> C {
  return holder.value
}
// CHECK-LABEL:sil hidden @_T09unmanaged3getAA1CCAA6HolderVz6holder_tF : $@convention(thin) (@inout Holder) -> @owned C
// CHECK: bb0([[ADDR:%.*]] : $*Holder):
// CHECK-NEXT:   debug_value_addr %0 : $*Holder, var, name "holder", argno 1 
// CHECK-NEXT:   [[T0:%.*]] = struct_element_addr [[ADDR]] : $*Holder, #Holder.value
// CHECK-NEXT:   [[T1:%.*]] = load [[T0]] : $*@sil_unmanaged C
// CHECK-NEXT:   [[T2:%.*]] = unmanaged_to_ref [[T1]]
// CHECK-NEXT:   strong_retain [[T2]]
// CHECK-NEXT:   return [[T2]]

func project(fn fn: () -> Holder) -> C {
  return fn().value
}
// CHECK-LABEL:sil hidden @_T09unmanaged7projectAA1CCAA6HolderVyc2fn_tF : $@convention(thin) (@owned @callee_owned () -> Holder) -> @owned C
// CHECK: bb0([[FN:%.*]] : $@callee_owned () -> Holder):
// CHECK:        strong_retain [[FN]]
// CHECK-NEXT: [[T0:%.*]] = apply [[FN]]()
// CHECK-NEXT: [[T1:%.*]] = struct_extract [[T0]] : $Holder, #Holder.value
// CHECK-NEXT: [[T2:%.*]] = unmanaged_to_ref [[T1]]
// CHECK-NEXT: strong_retain [[T2]]
// CHECK-NEXT: strong_release [[FN]]
// CHECK-NEXT: return [[T2]]
