// RUN: %swift -emit-silgen %s | FileCheck %s

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
// CHECK: sil hidden @_TF9accessors5test0FCS_1AT_ : $@thin (@owned A) -> () {
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
// CHECK-NEXT: strong_retain %0
// CHECK-NEXT: [[T0:%.*]] = class_method %0 : $A, #A.array!getter.1
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]](%0)
// CHECK-NEXT: store [[T1]] to [[TEMP]]
// CHECK-NEXT: [[T0:%.*]] = load [[TEMP]]
// CHECK-NEXT: // function_ref accessors.OrdinarySub.subscript.getter (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T1:%.*]] = function_ref @_TFV9accessors11OrdinarySubg9subscriptFSiSi
// CHECK-NEXT: [[VALUE:%.*]] = apply [[T1]]([[INDEX1]], [[T0]])
//   Formal access to LHS.
// CHECK-NEXT: [[BUFFER:%.*]] = alloc_stack $OrdinarySub
// CHECK-NEXT: strong_retain %0
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER]]
// CHECK-NEXT: [[T1:%.*]] = class_method %0 : $A, #A.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], %0)
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[SHOULD_WRITEBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: // function_ref accessors.OrdinarySub.subscript.setter (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFV9accessors11OrdinarySubs9subscriptFSiSi
// CHECK-NEXT: apply [[T0]]([[VALUE]], [[INDEX0]], [[T4]])
// CHECK-NEXT: cond_br [[SHOULD_WRITEBACK]], [[WRITEBACK:bb[0-9]+]], [[CONT:bb[0-9]+]]
// CHECK:    [[WRITEBACK]]:
// CHECK-NEXT: [[T0:%.*]] = load [[T4]] : $*OrdinarySub
// CHECK-NEXT: strong_retain %0
// CHECK-NEXT: [[T1:%.*]] = class_method %0 : $A, #A.array!setter.1
// CHECK-NEXT: apply [[T1]]([[T0]], %0)
// CHECK-NEXT: br [[CONT]]
// CHECK:    [[CONT]]:
// CHECK-NEXT: dealloc_stack [[BUFFER]]
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
// CHECK-LABEL: sil hidden @_TF9accessors5test1FCS_1BT_ : $@thin (@owned B) -> () {
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
// CHECK-NEXT: [[BUFFER:%.*]] = alloc_stack $MutatingSub
// CHECK-NEXT: strong_retain %0
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER]]
// CHECK-NEXT: [[T1:%.*]] = class_method %0 : $B, #B.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], %0)
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[SHOULD_WRITEBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: // function_ref accessors.MutatingSub.subscript.getter (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFV9accessors11MutatingSubg9subscriptFSiSi : $@cc(method) @thin (Int, @inout MutatingSub) -> Int 
// CHECK-NEXT: [[VALUE:%.*]] = apply [[T0]]([[INDEX1]], [[T4]])
// CHECK-NEXT: cond_br [[SHOULD_WRITEBACK]], [[WRITEBACK:bb[0-9]+]], [[CONT:bb[0-9]+]]
// CHECK:    [[WRITEBACK]]:
// CHECK-NEXT: [[T0:%.*]] = load [[T4]] : $*MutatingSub
// CHECK-NEXT: strong_retain %0
// CHECK-NEXT: [[T1:%.*]] = class_method %0 : $B, #B.array!setter.1
// CHECK-NEXT: apply [[T1]]([[T0]], %0)
// CHECK-NEXT: br [[CONT]]
// CHECK:    [[CONT]]:
//   Formal access to LHS.
// CHECK-NEXT: [[BUFFER2:%.*]] = alloc_stack $MutatingSub
// CHECK-NEXT: strong_retain %0
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER2]]
// CHECK-NEXT: [[T1:%.*]] = class_method %0 : $B, #B.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], %0)
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[SHOULD_WRITEBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: // function_ref accessors.MutatingSub.subscript.setter (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFV9accessors11MutatingSubs9subscriptFSiSi : $@cc(method) @thin (Int, Int, @inout MutatingSub) -> () 
// CHECK-NEXT: apply [[T0]]([[VALUE]], [[INDEX0]], [[T4]])
// CHECK-NEXT: cond_br [[SHOULD_WRITEBACK]], [[WRITEBACK:bb[0-9]+]], [[CONT:bb[0-9]+]]
// CHECK:    [[WRITEBACK]]:
// CHECK-NEXT: [[T0:%.*]] = load [[T4]] : $*MutatingSub
// CHECK-NEXT: strong_retain %0
// CHECK-NEXT: [[T1:%.*]] = class_method %0 : $B, #B.array!setter.1
// CHECK-NEXT: apply [[T1]]([[T0]], %0)
// CHECK-NEXT: br [[CONT]]
// CHECK:    [[CONT]]:
// CHECK-NEXT: dealloc_stack [[BUFFER2]]
// CHECK-NEXT: dealloc_stack [[BUFFER]]
//   Balance out the +1 from the function parameter.
// CHECK-NEXT: strong_release %0
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return
