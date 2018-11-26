
// RUN: %target-swift-emit-silgen -parse-stdlib -parse-as-library -enable-sil-ownership -module-name Swift %s | %FileCheck %s

precedencegroup AssignmentPrecedence { assignment: true }

enum Optional<Wrapped> {
  case none
  case some(Wrapped)
}

enum Boolish {
  case falsy
  case truthy
}

// CHECK-LABEL: sil hidden @$ss13Boolish_casesyyF
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

// CHECK-LABEL: sil hidden @$ss16Optionable_casesyySiF
func Optionable_cases(_ x: Int) {

  // CHECK:       [[METATYPE:%.*]] = metatype $@thin Optionable.Type
  // CHECK:       [[FN:%.*]] = function_ref @$ss10OptionableO4mereyABSicABmF
  // CHECK-NEXT:  [[CTOR:%.*]] = apply [[FN]]([[METATYPE]])
  // CHECK-NEXT:  destroy_value [[CTOR]]
  _ = Optionable.mere

  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin Optionable.Type
  // CHECK-NEXT:  [[RES:%.*]] = enum $Optionable, #Optionable.mere!enumelt.1, %0 : $Int
  _ = Optionable.mere(x)
}

// CHECK-LABEL: sil shared [transparent] [thunk] @$ss10OptionableO4mereyABSicABmF
// CHECK:        [[FN:%.*]] = function_ref @$ss10OptionableO4mereyABSicABmF
// CHECK-NEXT:   [[METHOD:%.*]] = partial_apply [callee_guaranteed] [[FN]](%0)
// CHECK-NEXT:   return [[METHOD]]
// CHECK-NEXT: }

// CHECK-LABEL: sil shared [transparent] @$ss10OptionableO4mereyABSicABmF
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

// CHECK-LABEL: sil hidden @$ss17AddressOnly_casesyys1SVF
func AddressOnly_cases(_ s: S) {

  // CHECK:       [[METATYPE:%.*]] = metatype $@thin AddressOnly.Type
  // CHECK:       [[FN:%.*]] = function_ref @$ss11AddressOnlyO4mereyABs1P_pcABmF
  // CHECK-NEXT:  [[CTOR:%.*]] = apply [[FN]]([[METATYPE]])
  // CHECK-NEXT:  destroy_value [[CTOR]]
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
  // CHECK-NEXT:  store %0 to [trivial] [[PAYLOAD_ADDR]]
  // CHECK-NEXT:  inject_enum_addr [[MERE]]
  // CHECK-NEXT:  destroy_addr [[MERE]]
  // CHECK-NEXT:  dealloc_stack [[MERE]]
  _ = AddressOnly.mere(s)

  // Address-only enum vs loadable payload

  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin AddressOnly.Type
  // CHECK-NEXT:  [[PHANTOM:%.*]] = alloc_stack $AddressOnly
  // CHECK-NEXT:  [[PAYLOAD:%.*]] = init_enum_data_addr [[PHANTOM]] : $*AddressOnly, #AddressOnly.phantom!enumelt.1
  // CHECK-NEXT:  store %0 to [trivial] [[PAYLOAD]]
  // CHECK-NEXT:  inject_enum_addr [[PHANTOM]] : $*AddressOnly, #AddressOnly.phantom!enumelt.1
  // CHECK-NEXT:  destroy_addr [[PHANTOM]]
  // CHECK-NEXT:  dealloc_stack [[PHANTOM]]

  _ = AddressOnly.phantom(s)
  // CHECK:       return
}

// CHECK-LABEL: sil shared [transparent] [thunk] @$ss11AddressOnlyO4mereyABs1P_pcABmF
// CHECK:       [[FN:%.*]] = function_ref @$ss11AddressOnlyO4mereyABs1P_pcABmF
// CHECK-NEXT:  [[METHOD:%.*]] = partial_apply [callee_guaranteed] [[FN]](%0)
// CHECK-NEXT:  // function_ref
// CHECK-NEXT:  [[CANONICAL_THUNK_FN:%.*]] = function_ref @$ss1P_ps11AddressOnlyOIegir_sAA_pACIegnr_TR : $@convention(thin) (@in_guaranteed P, @guaranteed @callee_guaranteed (@in P) -> @out AddressOnly) -> @out AddressOnly
// CHECK-NEXT:  [[CANONICAL_THUNK:%.*]] = partial_apply [callee_guaranteed] [[CANONICAL_THUNK_FN]]([[METHOD]])
// CHECK-NEXT:  return [[CANONICAL_THUNK]] : $@callee_guaranteed (@in_guaranteed P) -> @out AddressOnly
// CHECK-NEXT: }

// CHECK-LABEL: sil shared [transparent] @$ss11AddressOnlyO4mereyABs1P_pcABmF : $@convention
// CHECK: bb0([[ARG0:%.*]] : @trivial $*AddressOnly, [[ARG1:%.*]] : @trivial $*P, [[ARG2:%.*]] : @trivial $@thin AddressOnly.Type):
// CHECK:        [[RET_DATA:%.*]] = init_enum_data_addr [[ARG0]] : $*AddressOnly, #AddressOnly.mere!enumelt.1
// CHECK-NEXT:   copy_addr [take] [[ARG1]] to [initialization] [[RET_DATA]] : $*P
// CHECK-NEXT:   inject_enum_addr [[ARG0]] : $*AddressOnly, #AddressOnly.mere!enumelt.1
// CHECK:        return
// CHECK-NEXT: } // end sil function '$ss11AddressOnlyO4mereyABs1P_pcABmF'

enum PolyOptionable<T> {
  case nought
  case mere(T)
}

// CHECK-LABEL: sil hidden @$ss20PolyOptionable_casesyyxlF
func PolyOptionable_cases<T>(_ t: T) {

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

// CHECK-NOT:    destroy_addr %0
// CHECK:         return

}

// The substituted type is loadable and trivial here

// CHECK-LABEL: sil hidden @$ss32PolyOptionable_specialized_casesyySiF
func PolyOptionable_specialized_cases(_ t: Int) {

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

// Curry Thunk for Foo.A(_:)
//
// CHECK-LABEL: sil shared [transparent] [thunk] @$ss3FooO1AyABs1P_p_SStcABmF
// CHECK:         [[FN:%.*]] = function_ref @$ss3FooO1AyABs1P_p_SStcABmF
// CHECK-NEXT:    [[METHOD:%.*]] = partial_apply [callee_guaranteed] [[FN]](%0)
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[CANONICAL_THUNK_FN:%.*]] = function_ref @$ss1P_pSSs3FooOIegixr_sAA_pSSACIegngr_TR : $@convention(thin) (@in_guaranteed P, @guaranteed String, @guaranteed @callee_guaranteed (@in P, @owned String) -> @out Foo) -> @out Foo
// CHECK-NEXT:    [[CANONICAL_THUNK:%.*]] = partial_apply [callee_guaranteed] [[CANONICAL_THUNK_FN]]([[METHOD]])
// CHECK-NEXT:    return [[CANONICAL_THUNK]]
// CHECK-NEXT:  }

// Foo.A(_:)
// CHECK-LABEL: sil shared [transparent] @$ss3FooO1AyABs1P_p_SStcABmF
// CHECK: bb0([[ARG0:%.*]] : @trivial $*Foo, [[ARG1:%.*]] : @trivial $*P, [[ARG2:%.*]] : @owned $String, [[ARG3:%.*]] : @trivial $@thin Foo.Type):
// CHECK:         [[PAYLOAD:%.*]] = init_enum_data_addr [[ARG0]] : $*Foo, #Foo.A!enumelt.1
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PAYLOAD]] : $*(P, String), 0
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PAYLOAD]] : $*(P, String), 1
// CHECK-NEXT:    copy_addr [take] [[ARG1]] to [initialization] [[LEFT]] : $*P
// CHECK-NEXT:    store [[ARG2]] to [init] [[RIGHT]]
// CHECK-NEXT:    inject_enum_addr [[ARG0]] : $*Foo, #Foo.A!enumelt.1
// CHECK:         return
// CHECK-NEXT:  } // end sil function '$ss3FooO1AyABs1P_p_SStcABmF'

func Foo_cases() {
  _ = Foo.A
}

enum Indirect<T> {
  indirect case payload((T, other: T))
  case none
}
// CHECK-LABEL: sil{{.*}} @{{.*}}makeIndirectEnum{{.*}} : $@convention(thin) <T> (@in_guaranteed T) -> @owned Indirect<T>
// CHECK: [[BOX:%.*]] = alloc_box $<τ_0_0> { var (τ_0_0, other: τ_0_0) } <T>
// CHECK: enum $Indirect<T>, #Indirect.payload!enumelt.1, [[BOX]] : $<τ_0_0> { var (τ_0_0, other: τ_0_0) } <T>
func makeIndirectEnum<T>(_ payload: T) -> Indirect<T> {
  return Indirect.payload((payload, other: payload))
}
