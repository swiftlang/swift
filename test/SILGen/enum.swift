// RUN: %target-swift-frontend -parse-stdlib -parse-as-library -emit-silgen -enable-sil-ownership -enable-resilience -module-name Swift %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden @$Ss16Optionable_casesyySiF
func Optionable_cases(_ x: Int) {

  // CHECK:       [[METATYPE:%.*]] = metatype $@thin Optionable.Type
  // CHECK:       [[FN:%.*]] = function_ref @$Ss10OptionableO4mereyABSicABmF
  // CHECK-NEXT:  [[CTOR:%.*]] = apply [[FN]]([[METATYPE]])
  // CHECK-NEXT:  destroy_value [[CTOR]]
  _ = Optionable.mere

  // CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin Optionable.Type
  // CHECK-NEXT:  [[RES:%.*]] = enum $Optionable, #Optionable.mere!enumelt.1, %0 : $Int
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

// CHECK-LABEL: sil hidden @$Ss32PolyOptionable_specialized_casesyySiF
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


public class SomeClass {}
@_nonfrozen public enum NonExhaustiveValues {
  case a, b
}
@_nonfrozen public enum NonExhaustivePayload {
  case a
  case b(SomeClass)
}

// Make the function inlineable to force it to deal with future cases.
@_inlineable public func testNonExhaustiveSwitch(
    _ value: NonExhaustiveValues, _ payloaded: NonExhaustivePayload) {
  // This particular test will not work in Swift 5, where a nonexhaustive switch
  // is disallowed by Sema. In Swift 4 mode it's just a warning.
  switch value {
  case .a: break
  case .b: break
  }

  switch payloaded {
  case .a: break
  case .b(_): break
  }
}

// CHECK-LABEL: sil [serialized] @$Ss23testNonExhaustiveSwitchyys0bC6ValuesO_s0bC7PayloadOtF
// CHECK: switch_enum_addr [[VALUE:%.+]] : $*NonExhaustiveValues, case #NonExhaustiveValues.a!enumelt: [[bb_a:[^ ]+]], case #NonExhaustiveValues.b!enumelt: [[bb_b:[^ ]+]], default [[bb_default:[^ ]+]]
// CHECK: [[bb_a]]:
// CHECK-NEXT: dealloc_stack [[VALUE]] : $*NonExhaustiveValues
// CHECK-NEXT: br [[bb_done:[^ ]+]]
// CHECK: [[bb_b]]:
// CHECK-NEXT: dealloc_stack [[VALUE]] : $*NonExhaustiveValues
// CHECK-NEXT: br [[bb_done]]
// CHECK: [[bb_default]]:
// CHECK-NEXT: builtin "int_trap"()
// CHECK-NEXT: unreachable
// CHECK: [[bb_done]]:
// CHECK: switch_enum_addr [[VALUE:%.+]] : $*NonExhaustivePayload, case #NonExhaustivePayload.a!enumelt: [[bb_a:[^ ]+]], case #NonExhaustivePayload.b!enumelt.1: [[bb_b:[^ ]+]], default [[bb_default:[^ ]+]]
// CHECK: [[bb_a]]:
// CHECK-NEXT: dealloc_stack [[VALUE]] : $*NonExhaustivePayload
// CHECK-NEXT: br [[bb_done:[^ ]+]]
// CHECK: [[bb_b]]:
// CHECK-NEXT: [[PAYLOAD:%.+]] = unchecked_take_enum_data_addr [[VALUE]] : $*NonExhaustivePayload, #NonExhaustivePayload.b!enumelt.1
// CHECK-NEXT: [[REF:%.+]] = load [take] [[PAYLOAD]] : $*SomeClass
// CHECK-NEXT: destroy_value [[REF]] : $SomeClass
// CHECK-NEXT: dealloc_stack [[VALUE]] : $*NonExhaustivePayload
// CHECK: br [[bb_done]]
// CHECK: [[bb_default]]:
// CHECK-NEXT: builtin "int_trap"()
// CHECK-NEXT: unreachable
// CHECK: [[bb_done]]:
// CHECK: return
// CHECK: } // end sil function '$Ss23testNonExhaustiveSwitchyys0bC6ValuesO_s0bC7PayloadOtF'

public func testNonExhaustiveSwitchWithinModule(
    _ value: NonExhaustiveValues, _ payloaded: NonExhaustivePayload) {
  // This particular test will not work in Swift 5, where a nonexhaustive switch
  // is disallowed by Sema. In Swift 4 mode it's just a warning.
  switch value {
  case .a: break
  case .b: break
  }

  switch payloaded {
  case .a: break
  case .b(_): break
  }
}

// CHECK-LABEL: sil @$Ss35testNonExhaustiveSwitchWithinModuleyys0bC6ValuesO_s0bC7PayloadOtF
// CHECK: switch_enum_addr [[VALUE:%.+]] : $*NonExhaustiveValues, case #NonExhaustiveValues.a!enumelt: [[bb_a:[^ ]+]], case #NonExhaustiveValues.b!enumelt: [[bb_b:[^ ]+]]
// CHECK-NOT: default
// CHECK: [[bb_a]]:
// CHECK-NEXT: dealloc_stack [[VALUE]] : $*NonExhaustiveValues
// CHECK-NEXT: br [[bb_done:[^ ]+]]
// CHECK: [[bb_b]]:
// CHECK-NEXT: dealloc_stack [[VALUE]] : $*NonExhaustiveValues
// CHECK-NEXT: br [[bb_done]]
// CHECK: [[bb_done]]:
// CHECK: switch_enum_addr [[VALUE:%.+]] : $*NonExhaustivePayload, case #NonExhaustivePayload.a!enumelt: [[bb_a:[^ ]+]], case #NonExhaustivePayload.b!enumelt.1: [[bb_b:[^ ]+]]
// CHECK-NOT: default
// CHECK: [[bb_a]]:
// CHECK-NEXT: dealloc_stack [[VALUE]] : $*NonExhaustivePayload
// CHECK-NEXT: br [[bb_done:[^ ]+]]
// CHECK: [[bb_b]]:
// CHECK-NEXT: [[PAYLOAD:%.+]] = unchecked_take_enum_data_addr [[VALUE]] : $*NonExhaustivePayload, #NonExhaustivePayload.b!enumelt.1
// CHECK-NEXT: [[REF:%.+]] = load [take] [[PAYLOAD]] : $*SomeClass
// CHECK-NEXT: destroy_value [[REF]] : $SomeClass
// CHECK-NEXT: dealloc_stack [[VALUE]] : $*NonExhaustivePayload
// CHECK: br [[bb_done]]
// CHECK: [[bb_done]]:
// CHECK: return
// CHECK: } // end sil function '$Ss35testNonExhaustiveSwitchWithinModuleyys0bC6ValuesO_s0bC7PayloadOtF'
