// REQUIRES: plus_one_runtime

// RUN: %target-swift-frontend -parse-stdlib -parse-as-library -emit-silgen -enable-sil-ownership -module-name Swift %s | %FileCheck %s

precedencegroup AssignmentPrecedence { assignment: true }

enum Optional<Wrapped> {
  case none
  case some(Wrapped)
}

enum Boolish {
  case falsy
  case truthy
}

// CHECK-LABEL: sil hidden @$Ss13Boolish_casesyyF
func Boolish_cases() {
  // CHECK:       [[BOOLISH:%[0-9]+]] = metatype $@thin Boolish.Type
  // CHECK-NEXT:  // function_ref Boolish.falsy(_:)
  // CHECK-NEXT:  [[ENUM_CASE:%[0-9]+]] = function_ref @$Ss7BoolishO5falsyyA2BmF
  // CHECK-NEXT:  [[FALSY:%[0-9]+]] = apply [[ENUM_CASE]]([[BOOLISH]])
  _ = Boolish.falsy

  // CHECK-NEXT:  [[BOOLISH:%[0-9]+]] = metatype $@thin Boolish.Type
  // CHECK-NEXT:  // function_ref Boolish.truthy(_:)
  // CHECK-NEXT:  [[ENUM_CASE:%[0-9]+]] = function_ref @$Ss7BoolishO6truthyyA2BmF
  // CHECK-NEXT:  [[TRUTHY:%[0-9]+]] = apply [[ENUM_CASE]]([[BOOLISH]])
  _ = Boolish.truthy
}

struct Int {}

enum Optionable {
  case nought
  case mere(Int)
}

// CHECK-LABEL: sil hidden @$Ss16Optionable_casesyySiF
func Optionable_cases(_ x: Int) {

  // CHECK:       [[METATYPE:%.*]] = metatype $@thin Optionable.Type
  // CHECK:       [[FN:%.*]] = function_ref @$Ss10OptionableO4mereyABSicABmF
  // CHECK-NEXT:  [[CTOR:%.*]] = apply [[FN]]([[METATYPE]])
  // CHECK-NEXT:  destroy_value [[CTOR]]
  _ = Optionable.mere

  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin Optionable.Type
  // CHECK-NEXT:  // function_ref Optionable.mere(_:)
  // CHECK-NEXT:  [[ENUM_CASE:%.*]] = function_ref @$Ss10OptionableO4mereyABSicABmF
  // CHECK-NEXT:  [[ENUM_ELT:%[0-9]+]] = apply [[ENUM_CASE]](%0, [[METATYPE]])
  _ = Optionable.mere(x)
}

// CHECK-LABEL: sil shared [transparent] [thunk] @$Ss10OptionableO4mereyABSicABmF
// CHECK:        [[FN:%.*]] = function_ref @$Ss10OptionableO4mereyABSicABmF
// CHECK-NEXT:   [[METHOD:%.*]] = partial_apply [callee_guaranteed] [[FN]](%0)
// CHECK-NEXT:   return [[METHOD]]
// CHECK-NEXT: }

// CHECK-LABEL: sil shared [transparent] @$Ss10OptionableO4mereyABSicABmF
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

// CHECK-LABEL: sil hidden @$Ss17AddressOnly_casesyys1SVF
func AddressOnly_cases(_ s: S) {

  // CHECK:       [[METATYPE:%.*]] = metatype $@thin AddressOnly.Type
  // CHECK:       [[FN:%.*]] = function_ref @$Ss11AddressOnlyO4mereyABs1P_pcABmF
  // CHECK-NEXT:  [[CTOR:%.*]] = apply [[FN]]([[METATYPE]])
  // CHECK-NEXT:  destroy_value [[CTOR]]
  _ = AddressOnly.mere

  // CHECK-NEXT:  [[NOUGHT:%.*]] = alloc_stack $AddressOnly
  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin AddressOnly.Type
  // CHECK-NEXT:  // function_ref AddressOnly.nought(_:)
  // CHECK-NEXT:  [[ENUM_CASE:%[0-9]+]] = function_ref @$Ss11AddressOnlyO6noughtyA2BmF
  // CHECK-NEXT:  [[ENUM_ELT:%[0-9]+]] = apply [[ENUM_CASE]]([[NOUGHT]], [[METATYPE]])
  // CHECK-NEXT:  destroy_addr [[NOUGHT]]
  // CHECK-NEXT:  dealloc_stack [[NOUGHT]]
  _ = AddressOnly.nought

  // CHECK-NEXT:  [[MERE:%.*]] = alloc_stack $AddressOnly
  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin AddressOnly.Type
  // CHECK-NEXT:  [[PAYLOAD:%.*]] = alloc_stack $P
  // CHECK-NEXT:  [[PAYLOAD_ADDR:%.*]] = init_existential_addr [[PAYLOAD]]
  // CHECK-NEXT:  store %0 to [trivial] [[PAYLOAD_ADDR]]
  // CHECK-NEXT:  // function_ref AddressOnly.mere(_:)
  // CHECK-NEXT:  [[ENUM_CASE:%[0-9]+]] = function_ref @$Ss11AddressOnlyO4mereyABs1P_pcABmF
  // CHECK-NEXT:  [[ENUM_ELT:%[0-9]+]] = apply [[ENUM_CASE]]([[MERE]], [[PAYLOAD]], [[METATYPE]])
  // CHECK-NEXT:  dealloc_stack [[PAYLOAD]]
  // CHECK-NEXT:  destroy_addr [[MERE]]
  // CHECK-NEXT:  dealloc_stack [[MERE]]
  _ = AddressOnly.mere(s)

  // Address-only enum vs loadable payload

  // CHECK-NEXT:  [[PHANTOM:%.*]] = alloc_stack $AddressOnly
  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin AddressOnly.Type
  // CHECK-NEXT:  // function_ref AddressOnly.phantom(_:)
  // CHECK-NEXT:  [[ENUM_CASE:%[0-9]+]] = function_ref @$Ss11AddressOnlyO7phantomyABs1SVcABmF
  // CHECK-NEXT:  [[ENUM_ELT:%[0-9]+]] = apply [[ENUM_CASE]]([[PHANTOM]], %0, [[METATYPE]])
  // CHECK-NEXT:  destroy_addr [[PHANTOM]]
  // CHECK-NEXT:  dealloc_stack [[PHANTOM]]

  _ = AddressOnly.phantom(s)
  // CHECK:       return
}

// CHECK-LABEL: sil shared [transparent] [thunk] @$Ss11AddressOnlyO4mereyABs1P_pcABmF
// CHECK:       [[FN:%.*]] = function_ref @$Ss11AddressOnlyO4mereyABs1P_pcABmF
// CHECK-NEXT:  [[METHOD:%.*]] = partial_apply [callee_guaranteed] [[FN]](%0)
// CHECK-NEXT:  return [[METHOD]] : $@callee_guaranteed (@in P) -> @out AddressOnly
// CHECK-NEXT: }

// CHECK-LABEL: sil shared [transparent] @$Ss11AddressOnlyO4mereyABs1P_pcABmF
// CHECK:        [[RET_DATA:%.*]] = init_enum_data_addr %0 : $*AddressOnly, #AddressOnly.mere!enumelt.1
// CHECK-NEXT:   copy_addr [take] %1 to [initialization] [[RET_DATA]] : $*P
// CHECK-NEXT:   inject_enum_addr %0 : $*AddressOnly, #AddressOnly.mere!enumelt.1
// CHECK:        return
// CHECK-NEXT: }

enum PolyOptionable<T> {
  case nought
  case mere(T)
}

// CHECK-LABEL: sil hidden @$Ss20PolyOptionable_casesyyxlF
func PolyOptionable_cases<T>(_ t: T) {

// CHECK:         [[NOUGHT:%.*]] = alloc_stack $PolyOptionable<T>
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin PolyOptionable<T>.Type
// CHECK-NEXT:    // function_ref PolyOptionable.nought<A>(_:)
// CHECK-NEXT:    [[ENUM_CASE:%[0-9]+]] = function_ref @$Ss14PolyOptionableO6noughtyAByxGADmlF
// CHECK-NEXT:    [[ENUM_ELT:%[0-9]+]] = apply [[ENUM_CASE]]<T>([[NOUGHT]], [[METATYPE]])
// CHECK-NEXT:    destroy_addr [[NOUGHT]]
// CHECK-NEXT:    dealloc_stack [[NOUGHT]]
  _ = PolyOptionable<T>.nought

// CHECK-NEXT:    [[MERE:%.*]] = alloc_stack $PolyOptionable<T>
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin PolyOptionable<T>.Type
// CHECK-NEXT:    [[PAYLOAD:%.*]] = alloc_stack $T
// CHECK-NEXT:    copy_addr %0 to [initialization] [[PAYLOAD]] : $*T
// CHECK-NEXT:    // function_ref PolyOptionable.mere<A>(_:)
// CHECK-NEXT:    [[ENUM_CASE:%[0-9]+]] = function_ref @$Ss14PolyOptionableO4mereyAByxGxcADmlF
// CHECK-NEXT:    [[ENUM_ELT:%[0-9]+]] = apply [[ENUM_CASE]]<T>([[MERE]], [[PAYLOAD]], [[METATYPE]])
// CHECK-NEXT:    dealloc_stack [[PAYLOAD]]
// CHECK-NEXT:    destroy_addr [[MERE]]
// CHECK-NEXT:    dealloc_stack [[MERE]]

  _ = PolyOptionable<T>.mere(t)

// CHECK-NEXT:    destroy_addr %0
// CHECK:         return

}

// The substituted type is loadable and trivial here

// CHECK-LABEL: sil hidden @$Ss32PolyOptionable_specialized_casesyySiF
func PolyOptionable_specialized_cases(_ t: Int) {

// CHECK:         [[NOUGHT:%.*]] = alloc_stack $PolyOptionable<Int>
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin PolyOptionable<Int>.Type
// CHECK-NEXT:    // function_ref PolyOptionable.nought<A>(_:)
// CHECK-NEXT:    [[ENUM_CASE:%[0-9]+]] = function_ref @$Ss14PolyOptionableO6noughtyAByxGADmlF
// CHECK-NEXT:    [[ENUM_ELT:%[0-9]+]] = apply [[ENUM_CASE]]<Int>([[NOUGHT]], [[METATYPE]])
// CHECK-NEXT:    [[RES:%.*]] = load [trivial] [[NOUGHT]]
// CHECK-NEXT:    dealloc_stack [[NOUGHT]]
  _ = PolyOptionable<Int>.nought

// CHECK-NEXT:    [[MERE:%.*]] = alloc_stack $PolyOptionable<Int>
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin PolyOptionable<Int>.Type
// CHECK-NEXT:    [[ARG_STACK:%.*]] = alloc_stack $Int
// CHECK-NEXT:    store %0 to [trivial] [[ARG_STACK]] : $*Int
// CHECK-NEXT:    // function_ref PolyOptionable.mere<A>(_:)
// CHECK-NEXT:    [[ENUM_CASE:%[0-9]+]] = function_ref @$Ss14PolyOptionableO4mereyAByxGxcADmlF
// CHECK-NEXT:    [[ENUM_ELT:%[0-9]+]] = apply [[ENUM_CASE]]<Int>([[MERE]], [[ARG_STACK]], [[METATYPE]])
// CHECK-NEXT:    dealloc_stack [[ARG_STACK]]
// CHECK-NEXT:    [[RES:%.*]] = load [trivial] [[MERE]]
// CHECK-NEXT:    dealloc_stack [[MERE]]
  _ = PolyOptionable<Int>.mere(t)

// CHECK:         return

}


// Regression test for a bug where temporary allocations created as a result of
// tuple implosion were not deallocated in enum constructors.
struct String { var ptr: Builtin.NativeObject }

enum Foo { case A(P, String) }

// CHECK-LABEL: sil shared [transparent] [thunk] @$Ss3FooO1AyABs1P_p_SStcABmF
// CHECK:         [[FN:%.*]] = function_ref @$Ss3FooO1AyABs1P_p_SStcABmF
// CHECK-NEXT:    [[METHOD:%.*]] = partial_apply [callee_guaranteed] [[FN]](%0)
// CHECK-NEXT:    return [[METHOD]]
// CHECK-NEXT:  }

// CHECK-LABEL: sil shared [transparent] @$Ss3FooO1AyABs1P_p_SStcABmF
// CHECK:         [[PAYLOAD:%.*]] = init_enum_data_addr %0 : $*Foo, #Foo.A!enumelt.1
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PAYLOAD]] : $*(P, String), 0
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PAYLOAD]] : $*(P, String), 1
// CHECK-NEXT:    copy_addr [take] %1 to [initialization] [[LEFT]] : $*P
// CHECK-NEXT:    store %2 to [init] [[RIGHT]]
// CHECK-NEXT:    inject_enum_addr %0 : $*Foo, #Foo.A!enumelt.1
// CHECK:         return
// CHECK-NEXT:  }

func Foo_cases() {
  _ = Foo.A
}

enum Indirect<T> {
  indirect case payload((T, other: T))
  case none
}
// CHECK-LABEL: sil{{.*}} @{{.*}}makeIndirectEnum{{.*}} : $@convention(thin) <T> (@in T) -> @owned Indirect<T>
// CHECK: [[BOX:%.*]] = alloc_box $<τ_0_0> { var (τ_0_0, other: τ_0_0) } <T>
// CHECK: enum $Indirect<T>, #Indirect.payload!enumelt.1, [[BOX]] : $<τ_0_0> { var (τ_0_0, other: τ_0_0) } <T>
func makeIndirectEnum<T>(_ payload: T) -> Indirect<T> {
  return Indirect.payload((payload, other: payload))
}
