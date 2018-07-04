
// RUN: %target-swift-emit-silgen -module-name accessors -Xllvm -sil-full-demangle -enable-sil-ownership %s | %FileCheck %s

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
// CHECK: sil hidden @$S9accessors5test0yyAA1ACF : $@convention(thin) (@guaranteed A) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $A):
// CHECK-NEXT: debug_value
//   Formal evaluation of LHS.
// CHECK-NEXT: // function_ref accessors.index0() -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @$S9accessors6index0SiyF
// CHECK-NEXT: [[INDEX0:%.*]] = apply [[T0]]()
//   Formal evaluation of RHS.
// CHECK-NEXT: // function_ref accessors.index1() -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @$S9accessors6index1SiyF
// CHECK-NEXT: [[INDEX1:%.*]] = apply [[T0]]()
//   Formal access to RHS.
// CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $OrdinarySub
// CHECK-NEXT: [[T0:%.*]] = class_method [[ARG]] : $A, #A.array!getter.1
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[ARG]])
// CHECK-NEXT: store [[T1]] to [init] [[TEMP]]
// CHECK-NEXT: [[T0:%.*]] = load_borrow [[TEMP]]
// CHECK-NEXT: // function_ref accessors.OrdinarySub.subscript.getter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T1:%.*]] = function_ref @$S9accessors11OrdinarySubVyS2icig
// CHECK-NEXT: [[VALUE:%.*]] = apply [[T1]]([[INDEX1]], [[T0]])
// CHECK-NEXT: end_borrow [[T0]] from [[TEMP]]
// CHECK-NEXT: destroy_addr [[TEMP]]
//   Formal access to LHS.
// CHECK-NEXT: [[STORAGE:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK-NEXT: [[BUFFER:%.*]] = alloc_stack $OrdinarySub
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER]]
// CHECK-NEXT: [[T1:%.*]] = class_method [[ARG]] : $A, #A.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], [[STORAGE]], [[ARG]])
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[OPT_CALLBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[ADDR:%.*]] = mark_dependence [[T4]] : $*OrdinarySub on [[ARG]] : $A
// CHECK-NEXT: [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[ADDR]] : $*OrdinarySub
// CHECK-NEXT: // function_ref accessors.OrdinarySub.subscript.setter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[SETTER:%.*]] = function_ref @$S9accessors11OrdinarySubVyS2icis
// CHECK-NEXT: apply [[SETTER]]([[VALUE]], [[INDEX0]], [[ACCESS]])
// CHECK-NEXT: switch_enum [[OPT_CALLBACK]] : $Optional<Builtin.RawPointer>, case #Optional.some!enumelt.1: [[WRITEBACK:bb[0-9]+]], case #Optional.none!enumelt: [[CONT:bb[0-9]+]]

// CHECK:    [[WRITEBACK]]([[CALLBACK_ADDR:%.*]] : @trivial $Builtin.RawPointer):
// CHECK-NEXT: [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_ADDR]] : $Builtin.RawPointer to $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @in_guaranteed A, @thick A.Type) -> ()
// CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $A
// CHECK-NEXT: store_borrow [[ARG]] to [[TEMP2]] : $*A
// CHECK-NEXT: [[T0:%.*]] = metatype $@thick A.Type
// CHECK-NEXT: [[T1:%.*]] = address_to_pointer [[ACCESS]] : $*OrdinarySub to $Builtin.RawPointer
// CHECK-NEXT: apply [[CALLBACK]]([[T1]], [[STORAGE]], [[TEMP2]], [[T0]])
// CHECK-NEXT: dealloc_stack [[TEMP2]]
// CHECK-NEXT: br [[CONT]]

// CHECK:    [[CONT]]:
// CHECK-NEXT: end_access [[ACCESS]] : $*OrdinarySub
// CHECK-NEXT: dealloc_stack [[BUFFER]]
// CHECK-NEXT: dealloc_stack [[STORAGE]]
// CHECK-NEXT: dealloc_stack [[TEMP]]
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
// CHECK-LABEL: sil hidden @$S9accessors5test1yyAA1BCF : $@convention(thin) (@guaranteed B) -> () {
// CHECK:    bb0([[ARG:%.*]] : @guaranteed $B):
// CHECK-NEXT: debug_value
//   Formal evaluation of LHS.
// CHECK-NEXT: // function_ref accessors.index0() -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @$S9accessors6index0SiyF
// CHECK-NEXT: [[INDEX0:%.*]] = apply [[T0]]()
//   Formal evaluation of RHS.
// CHECK-NEXT: // function_ref accessors.index1() -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @$S9accessors6index1SiyF
// CHECK-NEXT: [[INDEX1:%.*]] = apply [[T0]]()
//   Formal access to RHS.
// CHECK-NEXT: [[STORAGE:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK-NEXT: [[BUFFER:%.*]] = alloc_stack $MutatingSub
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER]]
// CHECK-NEXT: [[T1:%.*]] = class_method [[ARG]] : $B, #B.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], [[STORAGE]], [[ARG]])
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[OPT_CALLBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[ADDR:%.*]] = mark_dependence [[T4]] : $*MutatingSub on [[ARG]] : $B
// CHECK-NEXT: [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[ADDR]] : $*MutatingSub
// CHECK-NEXT: // function_ref accessors.MutatingSub.subscript.getter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @$S9accessors11MutatingSubVyS2icig : $@convention(method) (Int, @inout MutatingSub) -> Int
// CHECK-NEXT: [[VALUE:%.*]] = apply [[T0]]([[INDEX1]], [[ACCESS]])
// CHECK-NEXT: switch_enum [[OPT_CALLBACK]] : $Optional<Builtin.RawPointer>, case #Optional.some!enumelt.1: [[WRITEBACK:bb[0-9]+]], case #Optional.none!enumelt: [[CONT:bb[0-9]+]]
//
// CHECK:    [[WRITEBACK]]([[CALLBACK_ADDR:%.*]] : @trivial $Builtin.RawPointer):
// CHECK-NEXT: [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_ADDR]] : $Builtin.RawPointer to $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @in_guaranteed B, @thick B.Type) -> ()
// CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $B
// CHECK-NEXT: store_borrow [[ARG]] to [[TEMP2]] : $*B
// CHECK-NEXT: [[T0:%.*]] = metatype $@thick B.Type
// CHECK-NEXT: [[T1:%.*]] = address_to_pointer [[ACCESS]] : $*MutatingSub to $Builtin.RawPointer
// CHECK-NEXT: apply [[CALLBACK]]([[T1]], [[STORAGE]], [[TEMP2]], [[T0]])
// CHECK-NEXT: dealloc_stack [[TEMP2]]
// CHECK-NEXT: br [[CONT]]
//
// CHECK:    [[CONT]]:
//   Formal access to LHS.
// CHECK-NEXT: end_access [[ACCESS]] : $*MutatingSub
// CHECK-NEXT: [[STORAGE2:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK-NEXT: [[BUFFER2:%.*]] = alloc_stack $MutatingSub
// CHECK-NEXT: [[T0:%.*]] = address_to_pointer [[BUFFER2]]
// CHECK-NEXT: [[T1:%.*]] = class_method [[ARG]] : $B, #B.array!materializeForSet.1
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], [[STORAGE2]], [[ARG]])
// CHECK-NEXT: [[T3:%.*]] = tuple_extract [[T2]] {{.*}}, 0
// CHECK-NEXT: [[OPT_CALLBACK:%.*]] = tuple_extract [[T2]] {{.*}}, 1
// CHECK-NEXT: [[T4:%.*]] = pointer_to_address [[T3]]
// CHECK-NEXT: [[ADDR:%.*]] = mark_dependence [[T4]] : $*MutatingSub on [[ARG]] : $B
// CHECK-NEXT: [[ACCESS:%.*]] = begin_access [modify] [unsafe] [[ADDR]] : $*MutatingSub
// CHECK-NEXT: // function_ref accessors.MutatingSub.subscript.setter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[SETTER:%.*]] = function_ref @$S9accessors11MutatingSubVyS2icis : $@convention(method) (Int, Int, @inout MutatingSub) -> ()
// CHECK-NEXT: apply [[SETTER]]([[VALUE]], [[INDEX0]], [[ACCESS]])
// CHECK-NEXT: switch_enum [[OPT_CALLBACK]] : $Optional<Builtin.RawPointer>, case #Optional.some!enumelt.1: [[WRITEBACK:bb[0-9]+]], case #Optional.none!enumelt: [[CONT:bb[0-9]+]]
//
// CHECK:    [[WRITEBACK]]([[CALLBACK_ADDR:%.*]] : @trivial $Builtin.RawPointer):
// CHECK-NEXT: [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_ADDR]] : $Builtin.RawPointer to $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @in_guaranteed B, @thick B.Type) -> ()
// CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $B
// CHECK-NEXT: store_borrow [[ARG]] to [[TEMP2]] : $*B
// CHECK-NEXT: [[T0:%.*]] = metatype $@thick B.Type
// CHECK-NEXT: [[T1:%.*]] = address_to_pointer [[ACCESS]] : $*MutatingSub to $Builtin.RawPointer
// CHECK-NEXT: apply [[CALLBACK]]([[T1]], [[STORAGE2]], [[TEMP2]], [[T0]])
// CHECK-NEXT: dealloc_stack [[TEMP2]]
// CHECK-NEXT: br [[CONT]]
//
// CHECK:    [[CONT]]:
// CHECK-NEXT: end_access [[ACCESS]] : $*MutatingSub
// CHECK-NEXT: dealloc_stack [[BUFFER2]]
// CHECK-NEXT: dealloc_stack [[STORAGE2]]
// CHECK-NEXT: dealloc_stack [[BUFFER]]
// CHECK-NEXT: dealloc_stack [[STORAGE]]
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
// CHECK: sil hidden @$S9accessors8test_recySiAA8RecOuterVzF : $@convention(thin) (@inout RecOuter) -> Int {
// CHECK:   function_ref @$S9accessors8RecOuterV5innerAA0B5InnerVvlu : $@convention(method) (RecOuter) -> UnsafePointer<RecInner>

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
// CHECK: sil hidden @$S9accessors9test_rec2ySiAA9Rec2OuterVzF : $@convention(thin) (@inout Rec2Outer) -> Int {
// CHECK:   function_ref @$S9accessors9Rec2OuterV5innerAA0B5InnerVvau : $@convention(method) (@inout Rec2Outer) -> UnsafeMutablePointer<Rec2Inner>

struct Foo {
  private subscript(privateSubscript x: Void) -> Void {
    // CHECK-DAG: sil private @$S9accessors3FooV16privateSubscriptyyt_tc33_D7F31B09EE737C687DC580B2014D759CLlig : $@convention(method) (Foo) -> () {
    get {}
  }
  private(set) subscript(withPrivateSet x: Void) -> Void {
    // CHECK-DAG: sil hidden @$S9accessors3FooV14withPrivateSetyyt_tcig : $@convention(method) (Foo) -> () {
    get {}
    // CHECK-DAG: sil private @$S9accessors3FooV14withPrivateSetyyt_tcis : $@convention(method) (@inout Foo) -> () {
    set {}
  }
  subscript(withNestedClass x: Void) -> Void {
    // Check for initializer of NestedClass
    // CHECK-DAG: sil private @$S9accessors3FooV15withNestedClassyyt_tcig0dE0L_CAFycfc : $@convention(method) (@owned NestedClass) -> @owned NestedClass {
    class NestedClass {}
  }

  // CHECK-DAG: sil private @$S9accessors3FooV15privateVariable33_D7F31B09EE737C687DC580B2014D759CLLytvg : $@convention(method) (Foo) -> () {
  private var privateVariable: Void {
    return
  }
  private(set) var variableWithPrivateSet: Void {
    // CHECK-DAG: sil hidden @$S9accessors3FooV22variableWithPrivateSetytvg : $@convention(method) (Foo) -> () {
    get {}
    // CHECK-DAG: sil private @$S9accessors3FooV22variableWithPrivateSetytvs : $@convention(method) (@inout Foo) -> () {
    set {}
  }
  var propertyWithNestedClass: Void {
    // Check for initializer of NestedClass
    // CHECK-DAG: sil private @$S9accessors3FooV23propertyWithNestedClassytvg0eF0L_CAFycfc : $@convention(method) (@owned NestedClass) -> @owned NestedClass {
    class NestedClass {}
  }
}
