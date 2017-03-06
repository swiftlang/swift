// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s

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

func someValidPointer<T>() -> UnsafePointer<T> { fatalError() }
func someValidPointer<T>() -> UnsafeMutablePointer<T> { fatalError() }

// Verify that there is no unnecessary extra copy_value of ref.array.
// rdar://19002913
func test0(_ ref: A) {
  ref.array[index0()] = ref.array[index1()]
}
// CHECK: sil hidden @_T09accessors5test0yAA1ACF : $@convention(thin) (@owned A) -> () {
// CHECK: bb0([[ARG:%.*]] : $A):
// CHECK-NEXT: debug_value
//   Formal evaluation of LHS.
// CHECK-NEXT: [[BORROWED_ARG_LHS:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT: // function_ref accessors.index0 () -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_T09accessors6index0SiyF
// CHECK-NEXT: [[INDEX0:%.*]] = apply [[T0]]()
//   Formal evaluation of RHS.
// CHECK-NEXT: [[BORROWED_ARG_RHS:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT: // function_ref accessors.index1 () -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_T09accessors6index1SiyF
// CHECK-NEXT: [[INDEX1:%.*]] = apply [[T0]]()
//   Formal access to RHS.
// CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $OrdinarySub
// CHECK-NEXT: [[T0:%.*]] = class_method [[BORROWED_ARG_RHS]] : $A, #A.array!getter.1
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[BORROWED_ARG_RHS]])
// CHECK-NEXT: store [[T1]] to [init] [[TEMP]]
// CHECK-NEXT: [[T0:%.*]] = load_borrow [[TEMP]]
// CHECK-NEXT: // function_ref accessors.OrdinarySub.subscript.getter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T1:%.*]] = function_ref @_T09accessors11OrdinarySubV9subscriptS2icfg
// CHECK-NEXT: [[VALUE:%.*]] = apply [[T1]]([[INDEX1]], [[T0]])
// CHECK-NEXT: end_borrow [[T0]] from [[TEMP]]
// CHECK-NEXT: destroy_addr [[TEMP]]
//   Formal access to LHS.
// CHECK-NEXT: [[STORAGE:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK-NEXT: [[BUFFER:%.*]] = alloc_stack $OrdinarySub
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER]]
// CHECK-NEXT: [[T1:%.*]] = class_method [[BORROWED_ARG_LHS]] : $A, #A.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], [[STORAGE]], [[BORROWED_ARG_LHS]])
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[OPT_CALLBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[ADDR:%.*]] = mark_dependence [[T4]] : $*OrdinarySub on [[BORROWED_ARG_LHS]] : $A
// CHECK-NEXT: // function_ref accessors.OrdinarySub.subscript.setter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_T09accessors11OrdinarySubV9subscriptS2icfs
// CHECK-NEXT: apply [[T0]]([[VALUE]], [[INDEX0]], [[ADDR]])
// CHECK-NEXT: switch_enum [[OPT_CALLBACK]] : $Optional<Builtin.RawPointer>, case #Optional.some!enumelt.1: [[WRITEBACK:bb[0-9]+]], case #Optional.none!enumelt: [[CONT:bb[0-9]+]]

// CHECK:    [[WRITEBACK]]([[CALLBACK_ADDR:%.*]] : $Builtin.RawPointer):
// CHECK-NEXT: [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_ADDR]] : $Builtin.RawPointer to $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout A, @thick A.Type) -> ()
// CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $A
// SEMANTIC SIL TODO: This is an issue caused by the callback for materializeForSet in the class case taking the value as @inout when it should really take it as @guaranteed.
// CHECK-NEXT: store [[BORROWED_ARG_LHS]] to [init] [[TEMP2]] : $*A
// CHECK-NEXT: [[T0:%.*]] = metatype $@thick A.Type
// CHECK-NEXT: [[T1:%.*]] = address_to_pointer [[ADDR]] : $*OrdinarySub to $Builtin.RawPointer
// CHECK-NEXT: apply [[CALLBACK]]([[T1]], [[STORAGE]], [[TEMP2]], [[T0]])
// CHECK-NEXT: dealloc_stack [[TEMP2]]
// CHECK-NEXT: br [[CONT]]

// CHECK:    [[CONT]]:
// CHECK-NEXT: dealloc_stack [[BUFFER]]
// CHECK-NEXT: dealloc_stack [[STORAGE]]
// CHECK-NEXT: dealloc_stack [[TEMP]]
// CHECK-NEXT: end_borrow [[BORROWED_ARG_RHS]] from [[ARG]]
// CHECK-NEXT: end_borrow [[BORROWED_ARG_LHS]] from [[ARG]]
//   Balance out the +1 from the function parameter.
// CHECK-NEXT: destroy_value [[ARG]]
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

func test1(_ ref: B) {
  ref.array[index0()] = ref.array[index1()]
}
// CHECK-LABEL: sil hidden @_T09accessors5test1yAA1BCF : $@convention(thin) (@owned B) -> () {
// CHECK:    bb0([[ARG:%.*]] : $B):
// CHECK-NEXT: debug_value
//   Formal evaluation of LHS.
// CHECK-NEXT: [[BORROWED_ARG_LHS:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT: // function_ref accessors.index0 () -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_T09accessors6index0SiyF
// CHECK-NEXT: [[INDEX0:%.*]] = apply [[T0]]()
//   Formal evaluation of RHS.
// CHECK-NEXT: [[BORROWED_ARG_RHS:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT: // function_ref accessors.index1 () -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_T09accessors6index1SiyF
// CHECK-NEXT: [[INDEX1:%.*]] = apply [[T0]]()
//   Formal access to RHS.
// CHECK-NEXT: [[STORAGE:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK-NEXT: [[BUFFER:%.*]] = alloc_stack $MutatingSub
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER]]
// CHECK-NEXT: [[T1:%.*]] = class_method [[BORROWED_ARG_RHS]] : $B, #B.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], [[STORAGE]], [[BORROWED_ARG_RHS]])
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[OPT_CALLBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[ADDR:%.*]] = mark_dependence [[T4]] : $*MutatingSub on [[BORROWED_ARG_RHS]] : $B
// CHECK-NEXT: // function_ref accessors.MutatingSub.subscript.getter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_T09accessors11MutatingSubV9subscriptS2icfg : $@convention(method) (Int, @inout MutatingSub) -> Int 
// CHECK-NEXT: [[VALUE:%.*]] = apply [[T0]]([[INDEX1]], [[ADDR]])
// CHECK-NEXT: switch_enum [[OPT_CALLBACK]] : $Optional<Builtin.RawPointer>, case #Optional.some!enumelt.1: [[WRITEBACK:bb[0-9]+]], case #Optional.none!enumelt: [[CONT:bb[0-9]+]]
//
// CHECK:    [[WRITEBACK]]([[CALLBACK_ADDR:%.*]] : $Builtin.RawPointer):
// CHECK-NEXT: [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_ADDR]] : $Builtin.RawPointer to $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout B, @thick B.Type) -> ()
// CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $B
// CHECK-NEXT: store [[BORROWED_ARG_RHS]] to [init] [[TEMP2]] : $*B
// CHECK-NEXT: [[T0:%.*]] = metatype $@thick B.Type
// CHECK-NEXT: [[T1:%.*]] = address_to_pointer [[ADDR]] : $*MutatingSub to $Builtin.RawPointer
// CHECK-NEXT: apply [[CALLBACK]]([[T1]], [[STORAGE]], [[TEMP2]], [[T0]])
// CHECK-NEXT: dealloc_stack [[TEMP2]]
// CHECK-NEXT: br [[CONT]]
//
// CHECK:    [[CONT]]:
//   Formal access to LHS.
// CHECK-NEXT: [[STORAGE2:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK-NEXT: [[BUFFER2:%.*]] = alloc_stack $MutatingSub
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER2]]
// CHECK-NEXT: [[T1:%.*]] = class_method [[BORROWED_ARG_LHS]] : $B, #B.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], [[STORAGE2]], [[BORROWED_ARG_LHS]])
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[OPT_CALLBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[ADDR:%.*]] = mark_dependence [[T4]] : $*MutatingSub on [[BORROWED_ARG_LHS]] : $B
// CHECK-NEXT: // function_ref accessors.MutatingSub.subscript.setter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_T09accessors11MutatingSubV9subscriptS2icfs : $@convention(method) (Int, Int, @inout MutatingSub) -> () 
// CHECK-NEXT: apply [[T0]]([[VALUE]], [[INDEX0]], [[ADDR]])
// CHECK-NEXT: switch_enum [[OPT_CALLBACK]] : $Optional<Builtin.RawPointer>, case #Optional.some!enumelt.1: [[WRITEBACK:bb[0-9]+]], case #Optional.none!enumelt: [[CONT:bb[0-9]+]]
//
// CHECK:    [[WRITEBACK]]([[CALLBACK_ADDR:%.*]] : $Builtin.RawPointer):
// CHECK-NEXT: [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_ADDR]] : $Builtin.RawPointer to $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout B, @thick B.Type) -> ()
// CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $B
// CHECK-NEXT: store [[BORROWED_ARG_LHS]] to [init] [[TEMP2]] : $*B
// CHECK-NEXT: [[T0:%.*]] = metatype $@thick B.Type
// CHECK-NEXT: [[T1:%.*]] = address_to_pointer [[ADDR]] : $*MutatingSub to $Builtin.RawPointer
// CHECK-NEXT: apply [[CALLBACK]]([[T1]], [[STORAGE2]], [[TEMP2]], [[T0]])
// CHECK-NEXT: dealloc_stack [[TEMP2]]
// CHECK-NEXT: br [[CONT]]
//
// CHECK:    [[CONT]]:
// CHECK-NEXT: dealloc_stack [[BUFFER2]]
// CHECK-NEXT: dealloc_stack [[STORAGE2]]
// CHECK-NEXT: dealloc_stack [[BUFFER]]
// CHECK-NEXT: dealloc_stack [[STORAGE]]
// CHECK: end_borrow [[BORROWED_ARG_RHS]] from [[ARG]]
// CHECK: end_borrow [[BORROWED_ARG_LHS]] from [[ARG]]
//   Balance out the +1 from the function parameter.
// CHECK-NEXT: destroy_value [[ARG]]
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

struct RecInner {
  subscript(i: Int) -> Int {
    get { return i }
  }
}
struct RecOuter {
  var inner : RecInner {
    unsafeAddress { return someValidPointer() }
    unsafeMutableAddress { return someValidPointer() }
  }
}
func test_rec(_ outer: inout RecOuter) -> Int {
  return outer.inner[0]
}
// This uses the immutable addressor.
// CHECK: sil hidden @_T09accessors8test_recSiAA8RecOuterVzF : $@convention(thin) (@inout RecOuter) -> Int {
// CHECK:   function_ref @_T09accessors8RecOuterV5innerAA0B5InnerVflu : $@convention(method) (RecOuter) -> UnsafePointer<RecInner>

struct Rec2Inner {
  subscript(i: Int) -> Int {
    mutating get { return i }
  }
}
struct Rec2Outer {
  var inner : Rec2Inner {
    unsafeAddress { return someValidPointer() }
    unsafeMutableAddress { return someValidPointer() }
  }
}
func test_rec2(_ outer: inout Rec2Outer) -> Int {
  return outer.inner[0]
}
// This uses the mutable addressor.
// CHECK: sil hidden @_T09accessors9test_rec2SiAA9Rec2OuterVzF : $@convention(thin) (@inout Rec2Outer) -> Int {
// CHECK:   function_ref @_T09accessors9Rec2OuterV5innerAA0B5InnerVfau : $@convention(method) (@inout Rec2Outer) -> UnsafeMutablePointer<Rec2Inner>
