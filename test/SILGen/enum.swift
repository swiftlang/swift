// RUN: %target-swift-frontend -parse-as-library -parse-stdlib -emit-silgen %s | FileCheck %s

enum Boolish {
  case falsy
  case truthy
}

// CHECK-LABEL: sil hidden @_TF4enum13Boolish_casesFT_T_
func Boolish_cases() {
  // CHECK:       [[BOOLISH:%[0-9]+]] = metatype $@thin Boolish.Type
  // CHECK-NEXT:  [[FALSY:%[0-9]+]] = enum $Boolish, #Boolish.falsy!enumelt
  _ = Boolish.falsy

  // CHECK-NEXT:  [[BOOLISH:%[0-9]+]] = metatype $@thin Boolish.Type
  // CHECK-NEXT:  [[TRUTHY:%[0-9]+]] = enum $Boolish, #Boolish.truthy!enumelt
  _ = Boolish.truthy
}

struct Int {}

enum Optionable {
  case nought
  case mere(Int)
}

// CHECK-LABEL: sil hidden @_TF4enum16Optionable_casesFVS_3IntT_
func Optionable_cases(x: Int) {

  // CHECK:       [[FN:%.*]] = function_ref @_TFO4enum10Optionable4mereFMS0_FVS_3IntS0_
  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin Optionable.Type
  // CHECK-NEXT:  [[CTOR:%.*]] = apply [[FN]]([[METATYPE]])
  // CHECK-NEXT:  strong_release [[CTOR]]
  _ = Optionable.mere

  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin Optionable.Type
  // CHECK-NEXT:  [[RES:%.*]] = enum $Optionable, #Optionable.mere!enumelt.1, %0 : $Int
  _ = Optionable.mere(x)
}

// CHECK-LABEL: sil shared [transparent] @_TFO4enum10Optionable4mereFMS0_FVS_3IntS0_
// CHECK:        [[FN:%.*]] = function_ref @_TFO4enum10Optionable4merefMS0_FVS_3IntS0_
// CHECK-NEXT:   [[METHOD:%.*]] = partial_apply [[FN]](%0)
// CHECK-NEXT:   return [[METHOD]]
// CHECK-NEXT: }

// CHECK-LABEL: sil shared [transparent] @_TFO4enum10Optionable4merefMS0_FVS_3IntS0_
// CHECK:        [[RES:%.*]] = enum $Optionable, #Optionable.mere!enumelt.1, %0 : $Int
// CHECK-NEXT:   return [[RES]] : $Optionable
// CHECK-NEXT: }

protocol P {}
struct S : P {}

enum AddressOnly {
  case nought
  case mere(P)
  case phantom(S)
}

// CHECK-LABEL: sil hidden @_TF4enum17AddressOnly_casesFVS_1ST_
func AddressOnly_cases(s: S) {

  // CHECK:       [[FN:%.*]] = function_ref @_TFO4enum11AddressOnly4mereFMS0_FPS_1P_S0_
  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin AddressOnly.Type
  // CHECK-NEXT:  [[CTOR:%.*]] = apply [[FN]]([[METATYPE]])
  // CHECK-NEXT:  strong_release [[CTOR]]
  _ = AddressOnly.mere

  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin AddressOnly.Type
  // CHECK-NEXT:  [[NOUGHT:%.*]] = alloc_stack $AddressOnly
  // CHECK-NEXT:  inject_enum_addr [[NOUGHT]]
  // CHECK-NEXT:  destroy_addr [[NOUGHT]]
  // CHECK-NEXT:  dealloc_stack [[NOUGHT]]
  _ = AddressOnly.nought

  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin AddressOnly.Type
  // CHECK-NEXT:  [[MERE:%.*]] = alloc_stack $AddressOnly
  // CHECK-NEXT:  [[PAYLOAD:%.*]] = init_enum_data_addr [[MERE]]
  // CHECK-NEXT:  [[PAYLOAD_ADDR:%.*]] = init_existential_addr [[PAYLOAD]]
  // CHECK-NEXT:  store %0 to [[PAYLOAD_ADDR]]
  // CHECK-NEXT:  inject_enum_addr [[MERE]]
  // CHECK-NEXT:  destroy_addr [[MERE]]
  // CHECK-NEXT:  dealloc_stack [[MERE]]
  _ = AddressOnly.mere(s)

  // Address-only enum vs loadable payload

  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin AddressOnly.Type
  // CHECK-NEXT:  [[PHANTOM:%.*]] = alloc_stack $AddressOnly
  // CHECK-NEXT:  [[PAYLOAD:%.*]] = init_enum_data_addr [[PHANTOM]] : $*AddressOnly, #AddressOnly.phantom!enumelt.1
  // CHECK-NEXT:  store %0 to [[PAYLOAD]]
  // CHECK-NEXT:  inject_enum_addr [[PHANTOM]] : $*AddressOnly, #AddressOnly.phantom!enumelt.1
  // CHECK-NEXT:  destroy_addr [[PHANTOM]]
  // CHECK-NEXT:  dealloc_stack [[PHANTOM]]

  _ = AddressOnly.phantom(s)
  // CHECK:       return
}

// CHECK-LABEL: sil shared [transparent] @_TFO4enum11AddressOnly4mereFMS0_FPS_1P_S0_
// CHECK:       [[FN:%.*]] = function_ref @_TFO4enum11AddressOnly4merefMS0_FPS_1P_S0_
// CHECK-NEXT:  [[METHOD:%.*]] = partial_apply [[FN]](%0)
// CHECK-NEXT:  return [[METHOD]] : $@callee_owned (@out AddressOnly, @in P) -> ()
// CHECK-NEXT: }

// CHECK-LABEL: sil shared [transparent] @_TFO4enum11AddressOnly4merefMS0_FPS_1P_S0_
// CHECK:        [[RET_DATA:%.*]] = init_enum_data_addr %0 : $*AddressOnly, #AddressOnly.mere!enumelt.1
// CHECK-NEXT:   copy_addr [take] %1 to [initialization] [[RET_DATA]] : $*P
// CHECK-NEXT:   inject_enum_addr %0 : $*AddressOnly, #AddressOnly.mere!enumelt.1
// CHECK:        return
// CHECK-NEXT: }

enum PolyOptionable<T> {
  case nought
  case mere(T)
}

// CHECK-LABEL: sil hidden @_TF4enum20PolyOptionable_casesurFxT_
func PolyOptionable_cases<T>(t: T) {

// CHECK:         [[METATYPE:%.*]] = metatype $@thin PolyOptionable<T>.Type
// CHECK-NEXT:    [[NOUGHT:%.*]] = alloc_stack $PolyOptionable<T>
// CHECK-NEXT:    inject_enum_addr [[NOUGHT]]
// CHECK-NEXT:    destroy_addr [[NOUGHT]]
// CHECK-NEXT:    dealloc_stack [[NOUGHT]]
  _ = PolyOptionable<T>.nought

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin PolyOptionable<T>.Type
// CHECK-NEXT:    [[MERE:%.*]] = alloc_stack $PolyOptionable<T>
// CHECK-NEXT:    [[PAYLOAD:%.*]] = init_enum_data_addr [[MERE]]
// CHECK-NEXT:    copy_addr %0 to [initialization] [[PAYLOAD]]
// CHECK-NEXT:    inject_enum_addr [[MERE]]
// CHECK-NEXT:    destroy_addr [[MERE]]
// CHECK-NEXT:    dealloc_stack [[MERE]]

  _ = PolyOptionable<T>.mere(t)

// CHECK-NEXT:    destroy_addr %0
// CHECK:         return

}

// The substituted type is loadable and trivial here

// CHECK-LABEL: sil hidden @_TF4enum32PolyOptionable_specialized_casesFVS_3IntT_
func PolyOptionable_specialized_cases(t: Int) {

// CHECK:         [[METATYPE:%.*]] = metatype $@thin PolyOptionable<Int>.Type
// CHECK-NEXT:    [[NOUGHT:%.*]] = enum $PolyOptionable<Int>, #PolyOptionable.nought!enumelt
  _ = PolyOptionable<Int>.nought

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin PolyOptionable<Int>.Type
// CHECK-NEXT:    [[NOUGHT:%.*]] = enum $PolyOptionable<Int>, #PolyOptionable.mere!enumelt.1, %0
  _ = PolyOptionable<Int>.mere(t)

// CHECK:         return

}


// Regression test for a bug where temporary allocations created as a result of
// tuple implosion were not deallocated in enum constructors.
struct String { var ptr: Builtin.NativeObject }

enum Foo { case A(P, String) }

// CHECK-LABEL: sil shared [transparent] @_TFO4enum3Foo1AFMS0_FTPS_1P_VS_6String_S0_
// CHECK:         [[FN:%.*]] = function_ref @_TFO4enum3Foo1AfMS0_FTPS_1P_VS_6String_S0_
// CHECK-NEXT:    [[METHOD:%.*]] = partial_apply [[FN]](%0)
// CHECK-NEXT:    return [[METHOD]]
// CHECK-NEXT:  }

// CHECK-LABEL: sil shared [transparent] @_TFO4enum3Foo1AfMS0_FTPS_1P_VS_6String_S0_
// CHECK:         [[PAYLOAD:%.*]] = init_enum_data_addr %0 : $*Foo, #Foo.A!enumelt.1
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PAYLOAD]] : $*(P, String), 0
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PAYLOAD]] : $*(P, String), 1
// CHECK-NEXT:    copy_addr [take] %1 to [initialization] [[LEFT]] : $*P
// CHECK-NEXT:    store %2 to [[RIGHT]]
// CHECK-NEXT:    inject_enum_addr %0 : $*Foo, #Foo.A!enumelt.1
// CHECK:         return
// CHECK-NEXT:  }

func Foo_cases() {
  _ = Foo.A
}
