// RUN: %target-swift-emit-silgen -enable-experimental-feature VariadicGenerics %s | %FileCheck %s

func receive_simple<T...>(_ args: repeat each T) {}
func receive_simple_owned<T...>(_ args: repeat __owned each T) {}

// CHECK-LABEL: @$s4main12scalar_test01i1f1sySi_SfSStF
// CHECK:         [[PACK:%.*]] = alloc_pack $Pack{Int, Float, String}
//   Copy i into a temporary and insert that into the pack.
// CHECK-NEXT:    [[INT_TEMP:%.*]] = alloc_stack $Int
// CHECK-NEXT:    store %0 to [trivial] [[INT_TEMP]] : $*Int
// CHECK-NEXT:    [[INT_INDEX:%.*]] = scalar_pack_index 0 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[INT_TEMP]] : $*Int into [[INT_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Copy f into a temporary and insert that into the pack.
// CHECK-NEXT:    [[FLOAT_TEMP:%.*]] = alloc_stack $Float
// CHECK-NEXT:    store %1 to [trivial] [[FLOAT_TEMP]] : $*Float
// CHECK-NEXT:    [[FLOAT_INDEX:%.*]] = scalar_pack_index 1 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[FLOAT_TEMP]] : $*Float into [[FLOAT_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Copy s into a temporary and insert that into the pack.
//   TODO: do this with a borrow
// CHECK-NEXT:    [[STRING_TEMP:%.*]] = alloc_stack $String
// CHECK-NEXT:    [[STRING_COPY:%.*]] = copy_value %2 : $String
// CHECK-NEXT:    store [[STRING_COPY]] to [init] [[STRING_TEMP]] : $*String
// CHECK-NEXT:    [[STRING_INDEX:%.*]] = scalar_pack_index 2 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[STRING_TEMP]] : $*String into [[STRING_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Perform the call.
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4main14receive_simpleyyxxQplF : $@convention(thin) <τ_0_0...> (@pack_guaranteed Pack{repeat each τ_0_0}) -> ()
// CHECK-NEXT:    apply [[FN]]<Pack{Int, Float, String}>([[PACK]])
//   Clean up.
// CHECK-NEXT:    destroy_addr [[STRING_TEMP]] : $*String
// CHECK-NEXT:    dealloc_stack [[STRING_TEMP]] : $*String
// CHECK-NEXT:    dealloc_stack [[FLOAT_TEMP]] : $*Float
// CHECK-NEXT:    dealloc_stack [[INT_TEMP]] : $*Int
// CHECK-NEXT:    dealloc_pack [[PACK]] : $*Pack{Int, Float, String}
func scalar_test0(i: Int, f: Float, s: String) {
  receive_simple(i, f, s)
}

// CHECK-LABEL: @$s4main12scalar_test11i1f1sySi_SfSStF
// CHECK:         [[PACK:%.*]] = alloc_pack $Pack{Int, Float, String}
//   Copy i into a temporary and insert that into the pack.
// CHECK-NEXT:    [[INT_TEMP:%.*]] = alloc_stack $Int
// CHECK-NEXT:    store %0 to [trivial] [[INT_TEMP]] : $*Int
// CHECK-NEXT:    [[INT_INDEX:%.*]] = scalar_pack_index 0 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[INT_TEMP]] : $*Int into [[INT_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Copy f into a temporary and insert that into the pack.
// CHECK-NEXT:    [[FLOAT_TEMP:%.*]] = alloc_stack $Float
// CHECK-NEXT:    store %1 to [trivial] [[FLOAT_TEMP]] : $*Float
// CHECK-NEXT:    [[FLOAT_INDEX:%.*]] = scalar_pack_index 1 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[FLOAT_TEMP]] : $*Float into [[FLOAT_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Copy s into a temporary and insert that into the pack.
//   TODO: do this with a borrow
// CHECK-NEXT:    [[STRING_TEMP:%.*]] = alloc_stack $String
// CHECK-NEXT:    [[STRING_COPY:%.*]] = copy_value %2 : $String
// CHECK-NEXT:    store [[STRING_COPY]] to [init] [[STRING_TEMP]] : $*String
// CHECK-NEXT:    [[STRING_INDEX:%.*]] = scalar_pack_index 2 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[STRING_TEMP]] : $*String into [[STRING_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Perform the call.
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref  @$s4main20receive_simple_ownedyyxxQpnlF : $@convention(thin) <τ_0_0...> (@pack_owned Pack{repeat each τ_0_0}) -> ()
// CHECK-NEXT:    apply [[FN]]<Pack{Int, Float, String}>([[PACK]])
//   Clean up.
// CHECK-NEXT:    dealloc_stack [[STRING_TEMP]] : $*String
// CHECK-NEXT:    dealloc_stack [[FLOAT_TEMP]] : $*Float
// CHECK-NEXT:    dealloc_stack [[INT_TEMP]] : $*Int
// CHECK-NEXT:    dealloc_pack [[PACK]] : $*Pack{Int, Float, String}
func scalar_test1(i: Int, f: Float, s: String) {
  receive_simple_owned(i, f, s)
}

#if false
func pack_test0<T...>(args: repeat each T) {
  receive_simple(repeat each args)
}

func pack_test1<T...>(args: repeat each T) {
  receive_simple_owned(repeat each args)
}
#endif