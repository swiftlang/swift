// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -O -emit-sil -whole-module-optimization %s | %FileCheck %s

public class Base {
  public var stored: Int = 10
  public var computed: Int {
    get {
      return stored
    }
    set {
      stored = newValue
    }
  }
}

// CHECK-LABEL: sil [transparent] [fragile] @_T024devirt_materializeForSet4BaseC8computedSifmytfU_ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type) -> () {
// CHECK:   function_ref @_T024devirt_materializeForSet4BaseC8computedSifs : $@convention(method) (Int, @guaranteed Base) -> ()
// CHECK:   return
// CHECK: }

// CHECK-LABEL: sil [thunk] [always_inline] @_T024devirt_materializeForSet14takesBaseClassyAA0F0C1b_tF : $@convention(thin) (@owned Base) -> () {
// CHECK:   ref_element_addr %0 : $Base, #Base.stored
// CHECK:   builtin "sadd_with_overflow_Int64"
// CHECK:   builtin "sadd_with_overflow_Int64"
// CHECK:   return
// CHECK: }
public func takesBaseClass(b: Base) {
  b.stored += 1
  b.computed += 1
}

// CHECK-LABEL: sil @_T024devirt_materializeForSet23takesBaseClassArchetypeyx1b_tAA0F0CRbzlF : $@convention(thin) <T where T : Base> (@owned T) -> () {
// CHECK:   [[SELF:%.*]] = upcast %0 : $T
// CHECK:   ref_element_addr [[SELF]] : $Base, #Base.stored
// CHECK:   builtin "sadd_with_overflow_Int64"
// CHECK:   builtin "sadd_with_overflow_Int64"
// CHECK:   return
// CHECK: }
public func takesBaseClassArchetype<T : Base>(b: T) {
  b.stored += 1
  b.computed += 1
}

public protocol Proto {
  var stored: Int { get set }
  var computed: Int { get set }
}

extension Base : Proto {}

// CHECK-LABEL: sil [transparent] [fragile] [thunk] @_T024devirt_materializeForSet4BaseCAA5ProtoAaaDP6storedSifmTW : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK:   [[SELF:%.*]] = load %2 : $*Base
// CHECK:   ref_element_addr [[SELF]] : $Base, #Base.stored
// CHECK: }

// CHECK-LABEL: sil [transparent] [fragile] [thunk] @_T024devirt_materializeForSet4BaseCAA5ProtoAaaDP8computedSifmTW : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK:   function_ref @_T024devirt_materializeForSet4BaseC8computedSifg
// CHECK:   function_ref @_T024devirt_materializeForSet4BaseC8computedSifmytfU_
// CHECK:   return
// CHECK: }

public func takesProtoArchetype<T : Proto>(p: inout T) {
  p.stored += 1
  p.computed += 1
}

// CHECK-LABEL: sil @_T024devirt_materializeForSet032callsProtoArchetypeWithBaseClassG0yx1b_tAA0I0CRbzlF : $@convention(thin) <T where T : Base> (@owned T) -> () {

// FIXME: Nothing is devirtualized here

// CHECK:   function_ref @_T024devirt_materializeForSet19takesProtoArchetypeyxz1p_tAA0F0RzlF : $@convention(thin) <τ_0_0 where τ_0_0 : Proto> (@inout τ_0_0) -> ()

// CHECK:   return
// CHECK: }

public func callsProtoArchetypeWithBaseClassArchetype<T : Base>(b: T) {
  var bb = b
  takesProtoArchetype(p: &bb)
}

// CHECK-LABEL: sil @_T024devirt_materializeForSet32callsProtoArchetypeWithBaseClassyAA0I0C1b_tFTf4g_n : $@convention(thin) (@guaranteed Base) -> () {
// CHECK:   ref_element_addr %0 : $Base, #Base.stored
// CHECK:   builtin "sadd_with_overflow_Int64"

// FIXME: Callback is not inlined here

// CHECK:   [[CALLBACK:%.*]] = function_ref @_T024devirt_materializeForSet4BaseC8computedSifmytfU_ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type) -> ()
// CHECK:   [[CALLBACK_PTR:%.*]] = thin_function_to_pointer [[CALLBACK]] : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type) -> () to $Builtin.RawPointer
// CHECK:   [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_PTR]] : $Builtin.RawPointer to $@convention(witness_method) <τ_0_0 where τ_0_0 : Proto> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:   apply [[CALLBACK]]<Base>({{.*}}) : $@convention(witness_method) <τ_0_0 where τ_0_0 : Proto> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:   return
// CHECK: }
public func callsProtoArchetypeWithBaseClass(b: Base) {
  var bb = b
  takesProtoArchetype(p: &bb)
}
