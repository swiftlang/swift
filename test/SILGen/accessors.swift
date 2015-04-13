// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// Hold a reference to do to magically become non-POD.
class Reference {}

// A struct with a non-mutating getter and a mutating setter.
struct OrdinarySub {
  var ptr = Reference()
  subscript(value: Int) -> Int {
    get { return value }
    set {}
  }
}

class A { var array = OrdinarySub() }

func index0() -> Int { return 0 }
func index1() -> Int { return 1 }

// Verify that there is no unnecessary extra retain of ref.array.
// rdar://19002913
func test0(ref: A) {
  ref.array[index0()] = ref.array[index1()]
}
// CHECK: sil hidden @_TF9accessors5test0FCS_1AT_ : $@convention(thin) (@owned A) -> () {
// CHECK: bb0(%0 : $A):
// CHECK-NEXT: debug_value
//   Formal evaluation of LHS.
// CHECK-NEXT: // function_ref accessors.index0 () -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TF9accessors6index0FT_Si
// CHECK-NEXT: [[INDEX0:%.*]] = apply [[T0]]()
//   Formal evaluation of RHS.
// CHECK-NEXT: // function_ref accessors.index1 () -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TF9accessors6index1FT_Si
// CHECK-NEXT: [[INDEX1:%.*]] = apply [[T0]]()
//   Formal access to RHS.
// CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $OrdinarySub
// CHECK-NEXT: [[T0:%.*]] = class_method %0 : $A, #A.array!getter.1
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]](%0)
// CHECK-NEXT: store [[T1]] to [[TEMP]]
// CHECK-NEXT: [[T0:%.*]] = load [[TEMP]]
// CHECK-NEXT: // function_ref accessors.OrdinarySub.subscript.getter (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T1:%.*]] = function_ref @_TFV9accessors11OrdinarySubg9subscriptFSiSi
// CHECK-NEXT: [[VALUE:%.*]] = apply [[T1]]([[INDEX1]], [[T0]])
// CHECK-NEXT: release_value [[T0]]
//   Formal access to LHS.
// CHECK-NEXT: [[STORAGE:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK-NEXT: [[BUFFER:%.*]] = alloc_stack $OrdinarySub
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER]]
// CHECK-NEXT: [[T1:%.*]] = class_method %0 : $A, #A.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], [[STORAGE]]#1, %0)
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[OPT_CALLBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: [[ADDR:%.*]] = mark_dependence [[T4]] : $*OrdinarySub on %0 : $A
// CHECK-NEXT: // function_ref accessors.OrdinarySub.subscript.setter (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFV9accessors11OrdinarySubs9subscriptFSiSi
// CHECK-NEXT: apply [[T0]]([[VALUE]], [[INDEX0]], [[ADDR]])
// CHECK-NEXT: switch_enum [[OPT_CALLBACK]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout A, @thick A.Type) -> ()>, case #Optional.Some!enumelt.1: [[WRITEBACK:bb[0-9]+]], case #Optional.None!enumelt: [[CONT:bb[0-9]+]]
// CHECK:    [[WRITEBACK]]([[CALLBACK:%.*]] : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout A, @thick A.Type) -> ()):
// CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $A
// CHECK-NEXT: store %0 to [[TEMP2]]#1 : $*A
// CHECK-NEXT: [[T0:%.*]] = metatype $@thick A.Type
// CHECK-NEXT: [[T1:%.*]] = address_to_pointer [[ADDR]] : $*OrdinarySub to $Builtin.RawPointer
// CHECK-NEXT: apply [[CALLBACK]]([[T1]], [[STORAGE]]#1, [[TEMP2]]#1, [[T0]])
// CHECK-NEXT: dealloc_stack [[TEMP2]]#0
// CHECK-NEXT: br [[CONT]]
// CHECK:    [[CONT]]:
// CHECK-NEXT: dealloc_stack [[BUFFER]]
// CHECK-NEXT: dealloc_stack [[STORAGE]]
// CHECK-NEXT: dealloc_stack [[TEMP]]
//   Balance out the +1 from the function parameter.
// CHECK-NEXT: strong_release %0
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

// A struct with a mutating getter and a mutating setter.
struct MutatingSub {
  var ptr = Reference()
  subscript(value: Int) -> Int {
    mutating get { return value }
    set {}
  }
}
class B { var array = MutatingSub() }

func test1(ref: B) {
  ref.array[index0()] = ref.array[index1()]
}
// CHECK-LABEL: sil hidden @_TF9accessors5test1FCS_1BT_ : $@convention(thin) (@owned B) -> () {
// CHECK:    bb0(%0 : $B):
// CHECK-NEXT: debug_value
//   Formal evaluation of LHS.
// CHECK-NEXT: // function_ref accessors.index0 () -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TF9accessors6index0FT_Si
// CHECK-NEXT: [[INDEX0:%.*]] = apply [[T0]]()
//   Formal evaluation of RHS.
// CHECK-NEXT: // function_ref accessors.index1 () -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TF9accessors6index1FT_Si
// CHECK-NEXT: [[INDEX1:%.*]] = apply [[T0]]()
//   Formal access to RHS.
// CHECK-NEXT: [[STORAGE:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK-NEXT: [[BUFFER:%.*]] = alloc_stack $MutatingSub
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER]]
// CHECK-NEXT: [[T1:%.*]] = class_method %0 : $B, #B.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], [[STORAGE]]#1, %0)
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[OPT_CALLBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: [[ADDR:%.*]] = mark_dependence [[T4]] : $*MutatingSub on %0 : $B
// CHECK-NEXT: // function_ref accessors.MutatingSub.subscript.getter (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFV9accessors11MutatingSubg9subscriptFSiSi : $@convention(method) (Int, @inout MutatingSub) -> Int 
// CHECK-NEXT: [[VALUE:%.*]] = apply [[T0]]([[INDEX1]], [[ADDR]])
// CHECK-NEXT: switch_enum [[OPT_CALLBACK]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout B, @thick B.Type) -> ()>, case #Optional.Some!enumelt.1: [[WRITEBACK:bb[0-9]+]], case #Optional.None!enumelt: [[CONT:bb[0-9]+]]
// CHECK:    [[WRITEBACK]]([[CALLBACK:%.*]] : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout B, @thick B.Type) -> ()):
// CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $B
// CHECK-NEXT: store %0 to [[TEMP2]]#1 : $*B
// CHECK-NEXT: [[T0:%.*]] = metatype $@thick B.Type
// CHECK-NEXT: [[T1:%.*]] = address_to_pointer [[ADDR]] : $*MutatingSub to $Builtin.RawPointer
// CHECK-NEXT: apply [[CALLBACK]]([[T1]], [[STORAGE]]#1, [[TEMP2]]#1, [[T0]])
// CHECK-NEXT: dealloc_stack [[TEMP2]]#0
// CHECK-NEXT: br [[CONT]]
// CHECK:    [[CONT]]:
//   Formal access to LHS.
// CHECK-NEXT: [[STORAGE2:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK-NEXT: [[BUFFER2:%.*]] = alloc_stack $MutatingSub
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER2]]
// CHECK-NEXT: [[T1:%.*]] = class_method %0 : $B, #B.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], [[STORAGE2]]#1, %0)
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[OPT_CALLBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: [[ADDR:%.*]] = mark_dependence [[T4]] : $*MutatingSub on %0 : $B
// CHECK-NEXT: // function_ref accessors.MutatingSub.subscript.setter (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFV9accessors11MutatingSubs9subscriptFSiSi : $@convention(method) (Int, Int, @inout MutatingSub) -> () 
// CHECK-NEXT: apply [[T0]]([[VALUE]], [[INDEX0]], [[ADDR]])
// CHECK-NEXT: switch_enum [[OPT_CALLBACK]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout B, @thick B.Type) -> ()>, case #Optional.Some!enumelt.1: [[WRITEBACK:bb[0-9]+]], case #Optional.None!enumelt: [[CONT:bb[0-9]+]]
// CHECK:    [[WRITEBACK]]([[CALLBACK:%.*]] : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout B, @thick B.Type) -> ()):
// CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $B
// CHECK-NEXT: store %0 to [[TEMP2]]#1 : $*B
// CHECK-NEXT: [[T0:%.*]] = metatype $@thick B.Type
// CHECK-NEXT: [[T1:%.*]] = address_to_pointer [[ADDR]] : $*MutatingSub to $Builtin.RawPointer
// CHECK-NEXT: apply [[CALLBACK]]([[T1]], [[STORAGE2]]#1, [[TEMP2]]#1, [[T0]])
// CHECK-NEXT: dealloc_stack [[TEMP2]]#0
// CHECK-NEXT: br [[CONT]]
// CHECK:    [[CONT]]:
// CHECK-NEXT: dealloc_stack [[BUFFER2]]
// CHECK-NEXT: dealloc_stack [[STORAGE2]]
// CHECK-NEXT: dealloc_stack [[BUFFER]]
// CHECK-NEXT: dealloc_stack [[STORAGE]]
//   Balance out the +1 from the function parameter.
// CHECK-NEXT: strong_release %0
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

struct RecInner {
  subscript(i: Int) -> Int {
    get { return i }
  }
}
struct RecOuter {
  var inner : RecInner {
    unsafeAddress { return nil }
    unsafeMutableAddress { return nil }
  }
}
func test_rec(inout outer: RecOuter) -> Int {
  return outer.inner[0]
}
// This uses the immutable addressor.
// CHECK: sil hidden @_TF9accessors8test_recFRVS_8RecOuterSi : $@convention(thin) (@inout RecOuter) -> Int {
// CHECK:   function_ref @_TFV9accessors8RecOuterlu5innerVS_8RecInner : $@convention(method) (RecOuter) -> UnsafePointer<RecInner>

struct Rec2Inner {
  subscript(i: Int) -> Int {
    mutating get { return i }
  }
}
struct Rec2Outer {
  var inner : Rec2Inner {
    unsafeAddress { return nil }
    unsafeMutableAddress { return nil }
  }
}
func test_rec2(inout outer: Rec2Outer) -> Int {
  return outer.inner[0]
}
// This uses the mutable addressor.
// CHECK: sil hidden @_TF9accessors9test_rec2FRVS_9Rec2OuterSi : $@convention(thin) (@inout Rec2Outer) -> Int {
// CHECK:   function_ref @_TFV9accessors9Rec2Outerau5innerVS_9Rec2Inner : $@convention(method) (@inout Rec2Outer) -> UnsafeMutablePointer<Rec2Inner>
