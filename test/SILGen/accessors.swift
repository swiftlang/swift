
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name accessors -Xllvm -sil-full-demangle %s | %FileCheck %s

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
// CHECK: sil hidden [ossa] @$s9accessors5test0yyAA1ACF : $@convention(thin) (@guaranteed A) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $A):
// CHECK-NEXT: debug_value
//   Formal evaluation of LHS.
// CHECK-NEXT: // function_ref accessors.index0() -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @$s9accessors6index0SiyF
// CHECK-NEXT: [[INDEX0:%.*]] = apply [[T0]]()
//   Formal evaluation of RHS.
// CHECK-NEXT: // function_ref accessors.index1() -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @$s9accessors6index1SiyF
// CHECK-NEXT: [[INDEX1:%.*]] = apply [[T0]]()
//   Formal access to RHS.
// CHECK-NEXT: [[T0:%.*]] = class_method [[ARG]] : $A, #A.array!getter
// CHECK-NEXT: [[OWNED_SELF:%.*]] = apply [[T0]]([[ARG]])
// CHECK-NEXT: [[SELF:%.*]] = begin_borrow [[OWNED_SELF]]
// CHECK-NEXT: // function_ref accessors.OrdinarySub.subscript.getter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T1:%.*]] = function_ref @$s9accessors11OrdinarySubVyS2icig
// CHECK-NEXT: [[VALUE:%.*]] = apply [[T1]]([[INDEX1]], [[SELF]])
// CHECK-NEXT: end_borrow [[SELF]]
//   Formal access to LHS.
// CHECK-NEXT: [[T0:%.*]] = class_method [[ARG]] : $A, #A.array!modify
// CHECK-NEXT: ([[T1:%.*]], [[T2:%.*]]) = begin_apply [[T0]]([[ARG]])
// CHECK-NEXT: // function_ref accessors.OrdinarySub.subscript.setter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[SETTER:%.*]] = function_ref @$s9accessors11OrdinarySubVyS2icis
// CHECK-NEXT: apply [[SETTER]]([[VALUE]], [[INDEX0]], [[T1]])
// CHECK-NEXT: end_apply [[T2]]
// CHECK-NEXT: destroy_value [[OWNED_SELF]]
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
// CHECK-LABEL: sil hidden [ossa] @$s9accessors5test1yyAA1BCF : $@convention(thin) (@guaranteed B) -> () {
// CHECK:    bb0([[ARG:%.*]] : @guaranteed $B):
// CHECK-NEXT: debug_value
//   Formal evaluation of LHS.
// CHECK-NEXT: // function_ref accessors.index0() -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @$s9accessors6index0SiyF
// CHECK-NEXT: [[INDEX0:%.*]] = apply [[T0]]()
//   Formal evaluation of RHS.
// CHECK-NEXT: // function_ref accessors.index1() -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @$s9accessors6index1SiyF
// CHECK-NEXT: [[INDEX1:%.*]] = apply [[T0]]()
//   Formal access to RHS.
// CHECK-NEXT: [[T0:%.*]] = class_method [[ARG]] : $B, #B.array!modify
// CHECK-NEXT: ([[T1:%.*]], [[TOKEN:%.*]]) = begin_apply [[T0]]([[ARG]])
// CHECK-NEXT: // function_ref accessors.MutatingSub.subscript.getter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[T0:%.*]] = function_ref @$s9accessors11MutatingSubVyS2icig : $@convention(method) (Int, @inout MutatingSub) -> Int
// CHECK-NEXT: [[VALUE:%.*]] = apply [[T0]]([[INDEX1]], [[T1]])
// CHECK-NEXT: end_apply [[TOKEN]]
//   Formal access to LHS.
// CHECK-NEXT: [[T0:%.*]] = class_method [[ARG]] : $B, #B.array!modify
// CHECK-NEXT: ([[T1:%.*]], [[TOKEN:%.*]]) = begin_apply [[T0]]([[ARG]])
// CHECK-NEXT: // function_ref accessors.MutatingSub.subscript.setter : (Swift.Int) -> Swift.Int
// CHECK-NEXT: [[SETTER:%.*]] = function_ref @$s9accessors11MutatingSubVyS2icis : $@convention(method) (Int, Int, @inout MutatingSub) -> ()
// CHECK-NEXT: apply [[SETTER]]([[VALUE]], [[INDEX0]], [[T1]])
// CHECK-NEXT: end_apply [[TOKEN]]
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
// CHECK: sil hidden [ossa] @$s9accessors8test_recySiAA8RecOuterVzF : $@convention(thin) (@inout RecOuter) -> Int {
// CHECK:   function_ref @$s9accessors8RecOuterV5innerAA0B5InnerVvlu : $@convention(method) (RecOuter) -> UnsafePointer<RecInner>

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
// CHECK: sil hidden [ossa] @$s9accessors9test_rec2ySiAA9Rec2OuterVzF : $@convention(thin) (@inout Rec2Outer) -> Int {
// CHECK:   function_ref @$s9accessors9Rec2OuterV5innerAA0B5InnerVvau : $@convention(method) (@inout Rec2Outer) -> UnsafeMutablePointer<Rec2Inner>

// https://github.com/apple/swift/issues/54895
// Compiler crash on 'class var' override adding observer

class OverrideAddingObserver_Base {
  open class var instance: OverrideAddingObserver_Base {
    get {
      return OverrideAddingObserver_Base()
    }
    set {}
  }
}

class OverrideAddingObserver_Derived : OverrideAddingObserver_Base {
  override class var instance: OverrideAddingObserver_Base {
    didSet {}
  }
}

// Make sure we can handle more complicated overrides.
class Parent<V> {
  class C<T, U> {
    class var foo: Int { get { 0 } set {} }
  }
  class D<T> : C<T, Int> {}
  class E : D<Double> {
    // CHECK-LABEL: sil hidden [ossa] @$s9accessors6ParentC1EC3fooSivgZ : $@convention(method) <V> (@thick Parent<V>.E.Type) -> Int
    // CHECK: [[TY:%.+]] = upcast {{%.+}} : $@thick Parent<V>.E.Type to $@thick Parent<V>.D<Double>.Type
    // CHECK: [[UPCASTTY:%.+]] = upcast [[TY]] : $@thick Parent<V>.D<Double>.Type to $@thick Parent<V>.C<Double, Int>.Type
    // CHECK: [[GETTER:%.+]] = function_ref @$s9accessors6ParentC1CC3fooSivgZ : $@convention(method) <τ_0_0><τ_1_0, τ_1_1> (@thick Parent<τ_0_0>.C<τ_1_0, τ_1_1>.Type) -> Int
    // CHECK: apply [[GETTER]]<V, Double, Int>([[UPCASTTY]])
    override class var foo: Int {
      didSet {}
    }
  }
}

struct Foo {
  private subscript(privateSubscript x: Void) -> Void {
    // CHECK-DAG: sil private [ossa] @$s9accessors3FooV16privateSubscriptyyt_tc33_D7F31B09EE737C687DC580B2014D759CLlig : $@convention(method) (Foo) -> () {
    get {}
  }
  private(set) subscript(withPrivateSet x: Void) -> Void {
    // CHECK-DAG: sil hidden [ossa] @$s9accessors3FooV14withPrivateSetyyt_tcig : $@convention(method) (Foo) -> () {
    get {}
    // CHECK-DAG: sil hidden [ossa] @$s9accessors3FooV14withPrivateSetyyt_tcis : $@convention(method) (@inout Foo) -> () {
    set {}
  }
  subscript(withNestedClass x: Void) -> Void {
    // Check for initializer of NestedClass
    // CHECK-DAG: sil private [ossa] @$s9accessors3FooV15withNestedClassyyt_tcig0dE0L_CAFycfc : $@convention(method) (@owned NestedClass) -> @owned NestedClass {
    class NestedClass {}
  }

  // CHECK-DAG: sil private [ossa] @$s9accessors3FooV15privateVariable33_D7F31B09EE737C687DC580B2014D759CLLytvg : $@convention(method) (Foo) -> () {
  private var privateVariable: Void {
    return
  }
  private(set) var variableWithPrivateSet: Void {
    // CHECK-DAG: sil hidden [ossa] @$s9accessors3FooV22variableWithPrivateSetytvg : $@convention(method) (Foo) -> () {
    get {}
    // CHECK-DAG: sil hidden [ossa] @$s9accessors3FooV22variableWithPrivateSetytvs : $@convention(method) (@inout Foo) -> () {
    set {}
  }
  var propertyWithNestedClass: Void {
    // Check for initializer of NestedClass
    // CHECK-DAG: sil private [ossa] @$s9accessors3FooV23propertyWithNestedClassytvg0eF0L_CAFycfc : $@convention(method) (@owned NestedClass) -> @owned NestedClass {
    class NestedClass {}
  }
}
