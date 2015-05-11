// RUN: %target-swift-frontend -emit-silgen %s -disable-objc-attr-requires-foundation-module | FileCheck %s

protocol Fooable {
  init()
  func foo(x: Int)
  mutating func bar()
  mutating func bas()

  var prop1: Int { get set }
  var prop2: Int { get set }
  var prop3: Int { get nonmutating set }
}

protocol Barrable: class {
  init()
  func foo(x: Int)
  func bar()
  func bas()

  var prop1: Int { get set }
  var prop2: Int { get set }
  var prop3: Int { get set }
}

struct S: Fooable {
  var x: C? // Make the type nontrivial, so +0/+1 is observable.

  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1SCfMS0_FT_S0_ : $@convention(thin) (@thin S.Type) -> @owned S
  init() {}
  // TODO: Way too many redundant r/r pairs here. Should use +0 rvalues.
  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1S3foofS0_FSiT_ : $@convention(method) (Int, @guaranteed S) -> () {
  // CHECK:       bb0({{.*}} [[SELF:%.*]] : $S):
  // CHECK-NOT:     retain_value [[SELF]]
  // CHECK-NOT:     release_value [[SELF]]
  func foo(x: Int) {
    self.foo(x)
  }

  func foooo(x: (Int, Bool)) {
    self.foooo(x)
  }

  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1S3barfRS0_FT_T_ : $@convention(method) (@inout S) -> ()
  // CHECK:       bb0([[SELF:%.*]] : $*S):
  // CHECK-NOT:     destroy_addr [[SELF]]
  mutating func bar() {
    self.bar()
  }
  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1S3basfS0_FT_T_ : $@convention(method) (@guaranteed S) -> ()
  // CHECK:       bb0([[SELF:%.*]] : $S):
  // CHECK-NOT:     retain_value [[SELF]]
  // CHECK-NOT:     release_value [[SELF]]
  func bas() {
    self.bas()
  }

  var prop1: Int = 0

  var prop2: Int {
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1Sg5prop2Si : $@convention(method) (@guaranteed S) -> Int
    // CHECK:       bb0([[SELF:%.*]] : $S):
    // CHECK-NOT:     release_value [[SELF]]
    get { return 0 }
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1Ss5prop2Si : $@convention(method) (Int, @inout S) -> ()
    // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self1Sm5prop2Si : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>)
    set { }
  }

  var prop3: Int {
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1Sg5prop3Si : $@convention(method) (@guaranteed S) -> Int
    // CHECK:       bb0([[SELF:%.*]] : $S):
    // CHECK-NOT:     release_value [[SELF]]
    get { return 0 }
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self1Ss5prop3Si : $@convention(method) (Int, @guaranteed S) -> ()
    // CHECK:       bb0({{.*}} [[SELF:%.*]] : $S):
    // CHECK-NOT:     release_value [[SELF]]
    // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self1Sm5prop3Si : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed S) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>)
    // CHECK:       bb0({{.*}} [[SELF:%.*]] : $S):
    // CHECK-NOT:     release_value [[SELF]]
    nonmutating set { }
  }

  // Getter for prop1
  // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self1Sg5prop1Si : $@convention(method) (@guaranteed S) -> Int
  // CHECK:       bb0([[SELF:%.*]] : $S):
  // CHECK-NOT:     release_value [[SELF]]

  // Setter for prop1
  // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self1Ss5prop1Si : $@convention(method) (Int, @inout S) -> ()
  // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
  // CHECK-NOT:     load [[SELF_ADDR]]
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]

  // materializeForSet for prop1
  // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self1Sm5prop1Si : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>)
  // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
  // CHECK-NOT:     load [[SELF_ADDR]]
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
}

// Witness thunk for nonmutating 'foo'
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_3foouRq_S1__fq_FSiT_ : $@convention(witness_method) (Int, @in_guaranteed S) -> () {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF_COPY:%.*]] = alloc_stack $S
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY]]
// CHECK:         [[SELF:%.*]] = load [[SELF_COPY]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for mutating 'bar'
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_3baruRq_S1__fRq_FT_T_ : $@convention(witness_method) (@inout S) -> () {
// CHECK:       bb0([[SELF_ADDR:%.*]] : $*S):
// CHECK-NOT:     load [[SELF_ADDR]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for 'bas', which is mutating in the protocol, but nonmutating
// in the implementation
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_3basuRq_S1__fRq_FT_T_ : $@convention(witness_method) (@inout S) -> ()
// CHECK:       bb0([[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF:%.*]] = load [[SELF_ADDR]]
// CHECK:         retain_value [[SELF]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]

// Witness thunk for prop1 getter
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_g5prop1Si : $@convention(witness_method) (@in_guaranteed S) -> Int
// CHECK:       bb0([[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF_COPY:%.*]] = alloc_stack $S
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY]]
// CHECK:         [[SELF:%.*]] = load [[SELF_COPY]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop1 setter
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_s5prop1Si : $@convention(witness_method) (Int, @inout S) -> () {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop1 materializeForSet
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_m5prop1Si : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>) {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop2 getter
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_g5prop2Si : $@convention(witness_method) (@in_guaranteed S) -> Int
// CHECK:       bb0([[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF_COPY:%.*]] = alloc_stack $S
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY]]
// CHECK:         [[SELF:%.*]] = load [[SELF_COPY]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop2 setter
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_s5prop2Si : $@convention(witness_method) (Int, @inout S) -> () {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop2 materializeForSet
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_m5prop2Si : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>) {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop3 getter
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_g5prop3Si : $@convention(witness_method) (@in_guaranteed S) -> Int
// CHECK:       bb0([[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF_COPY:%.*]] = alloc_stack $S
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY]]
// CHECK:         [[SELF:%.*]] = load [[SELF_COPY]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop3 nonmutating setter
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_s5prop3Si : $@convention(witness_method) (Int, @in_guaranteed S) -> ()
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF_COPY:%.*]] = alloc_stack $S
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY]]
// CHECK:         [[SELF:%.*]] = load [[SELF_COPY]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop3 nonmutating materializeForSet
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV15guaranteed_self1SS_7FooableS_FS1_m5prop3Si : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout S, @thick S.Type) -> ()>)
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*S):
// CHECK:         [[SELF:%.*]] = load [[SELF_ADDR]]
// CHECK:         retain_value [[SELF]]
// CHECK:         release_value [[SELF]]
// CHECK-NOT:     release_value [[SELF]]
// CHECK:       }

//
// TODO: Expected output for the other cases
//

struct AO<T>: Fooable {
  var x: T?

  init() {}
  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self2AO3foourfGS0_q__FSiT_ : $@convention(method) <T> (Int, @in_guaranteed AO<T>) -> ()
  // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*AO<T>):
  // CHECK-NOT:     copy_addr
  // CHECK:         apply {{.*}} [[SELF_ADDR]]
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
  // CHECK:       }
  func foo(x: Int) {
    self.foo(x)
  }
  mutating func bar() {
    self.bar()
  }
  // CHECK-LABEL: sil hidden @_TFV15guaranteed_self2AO3basurfGS0_q__FT_T_ : $@convention(method) <T> (@in_guaranteed AO<T>) -> ()
  // CHECK:       bb0([[SELF_ADDR:%.*]] : $*AO<T>):
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
  func bas() {
    self.bas()
  }


  var prop1: Int = 0
  var prop2: Int {
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self2AOg5prop2Si : $@convention(method) <T> (@in_guaranteed AO<T>) -> Int {
    // CHECK:       bb0([[SELF_ADDR:%.*]] : $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    get { return 0 }
    set { }
  }
  var prop3: Int {
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self2AOg5prop3Si : $@convention(method) <T> (@in_guaranteed AO<T>) -> Int
    // CHECK:       bb0([[SELF_ADDR:%.*]] : $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    get { return 0 }
    // CHECK-LABEL: sil hidden @_TFV15guaranteed_self2AOs5prop3Si : $@convention(method) <T> (Int, @in_guaranteed AO<T>) -> ()
    // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    // CHECK-LABEL: sil hidden [transparent] @_TFV15guaranteed_self2AOm5prop3Si : $@convention(method) <T> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @in_guaranteed AO<T>) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout AO<T>, @thick AO<T>.Type) -> ()>)
    // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    // CHECK:       }
    nonmutating set { }
  }
}

// Witness for nonmutating 'foo'
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWurGV15guaranteed_self2AOq__S_7FooableS_FS1_3foouRq_S1__fq_FSiT_ : $@convention(witness_method) <T> (Int, @in_guaranteed AO<T>) -> ()
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : $*AO<T>):
// TODO: This copy isn't necessary.
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY:%.*]]#1
// CHECK:         apply {{.*}} [[SELF_COPY]]
// CHECK:         destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness for 'bar', which is mutating in protocol but nonmutating in impl
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWurGV15guaranteed_self2AOq__S_7FooableS_FS1_3baruRq_S1__fRq_FT_T_ : $@convention(witness_method) <T> (@inout AO<T>) -> ()
// CHECK:       bb0([[SELF_ADDR:%.*]] : $*AO<T>):
// -- NB: This copy *is* necessary, unless we're willing to assume an inout
//        parameter is not mutably aliased.
// CHECK:         copy_addr [[SELF_ADDR]] to [initialization] [[SELF_COPY:%.*]]#1
// CHECK:         apply {{.*}} [[SELF_COPY]]
// CHECK:         destroy_addr [[SELF_COPY]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

class C: Fooable, Barrable {
  // Allocating initializer
  // CHECK-LABEL: sil hidden @_TFC15guaranteed_self1CCfMS0_FT_S0_ : $@convention(thin) (@thick C.Type) -> @owned C
  // CHECK:         [[SELF1:%.*]] = alloc_ref $C
  // CHECK-NOT:     [[SELF1]]
  // CHECK:         [[SELF2:%.*]] = apply {{.*}}([[SELF1]])
  // CHECK-NOT:     [[SELF2]]
  // CHECK:         return [[SELF2]]

  // Initializing constructors still have the +1 in, +1 out convention.
  // CHECK-LABEL: sil hidden @_TFC15guaranteed_self1CcfMS0_FT_S0_ : $@convention(method) (@owned C) -> @owned C {
  // CHECK:       bb0([[SELF:%.*]] : $C):
  // CHECK:         [[MARKED_SELF:%.*]] = mark_uninitialized [rootself] [[SELF]]
  // CHECK-NOT:     [[SELF]]
  // CHECK-NOT:     strong_retain [[MARKED_SELF]]
  // CHECK-NOT:     strong_release [[MARKED_SELF]]
  // CHECK:         return [[MARKED_SELF]]

  // @objc thunk for initializing constructor
  // CHECK-LABEL: sil hidden @_TToFC15guaranteed_self1CcfMS0_FT_S0_ : $@convention(objc_method) (@owned C) -> @owned C
  // CHECK:       bb0([[SELF:%.*]] : $C):
  // CHECK-NOT:     retain{{.*}} [[SELF]]
  // CHECK:         [[SELF2:%.*]] = apply {{%.*}}([[SELF]])
  // CHECK-NOT:     release{{.*}} [[SELF]]
  // CHECK-NOT:     release{{.*}} [[SELF2]]
  // CHECK:         return [[SELF2]]
  @objc required init() {}


  // CHECK-LABEL: sil hidden @_TFC15guaranteed_self1C3foofS0_FSiT_ : $@convention(method) (Int, @guaranteed C) -> ()
  // CHECK:       bb0({{.*}} [[SELF:%.*]] : $C):
  // CHECK-NOT:     retain
  // CHECK-NOT:     release

  // CHECK-LABEL: sil hidden @_TToFC15guaranteed_self1C3foofS0_FSiT_ : $@convention(objc_method) (Int, C) -> () {
  // CHECK:       bb0({{.*}} [[SELF:%.*]] : $C):
  // CHECK:         retain{{.*}} [[SELF]]
  // CHECK:         apply {{.*}} [[SELF]]
  // CHECK:         release{{.*}} [[SELF]]
  // CHECK-NOT:     release{{.*}} [[SELF]]
  @objc func foo(x: Int) {
    self.foo(x)
  }
  @objc func bar() {
    self.bar()
  }
  @objc func bas() {
    self.bas()
  }

  // CHECK-LABEL: sil hidden [transparent] @_TToFC15guaranteed_self1Cg5prop1Si : $@convention(objc_method) (C) -> Int
  // CHECK:       bb0([[SELF:%.*]] : $C):
  // CHECK:         retain{{.*}} [[SELF]]
  // CHECK:         apply {{.*}}([[SELF]])
  // CHECK:         release{{.*}} [[SELF]]
  // CHECK-NOT:     release{{.*}} [[SELF]]

  // CHECK-LABEL: sil hidden [transparent] @_TToFC15guaranteed_self1Cs5prop1Si : $@convention(objc_method) (Int, C) -> ()
  // CHECK:       bb0({{.*}} [[SELF:%.*]] : $C):
  // CHECK:         retain{{.*}} [[SELF]]
  // CHECK:         apply {{.*}} [[SELF]]
  // CHECK:         release{{.*}} [[SELF]]
  // CHECK-NOT:     release{{.*}} [[SELF]]
  // CHECK:       }
  @objc var prop1: Int = 0
  @objc var prop2: Int {
    get { return 0 }
    set {}
  }
  @objc var prop3: Int {
    get { return 0 }
    set {}
  }

}

class D: C {
  // CHECK-LABEL: sil hidden @_TFC15guaranteed_self1DCfMS0_FT_S0_ : $@convention(thin) (@thick D.Type) -> @owned D
  // CHECK:         [[SELF1:%.*]] = alloc_ref $D
  // CHECK-NOT:     [[SELF1]]
  // CHECK:         [[SELF2:%.*]] = apply {{.*}}([[SELF1]])
  // CHECK-NOT:     [[SELF1]]
  // CHECK-NOT:     [[SELF2]]
  // CHECK:         return [[SELF2]]

  // CHECK-LABEL: sil hidden @_TFC15guaranteed_self1DcfMS0_FT_S0_ : $@convention(method) (@owned D) -> @owned D
  // CHECK:       bb0([[SELF:%.*]] : $D):
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box $D
  // CHECK-NEXT:    [[SELF_ADDR:%.*]] = mark_uninitialized [derivedself] [[SELF_BOX]]
  // CHECK-NEXT:    store [[SELF]] to [[SELF_ADDR]]
  // CHECK-NOT:     [[SELF_ADDR]]
  // CHECK:         [[SELF1:%.*]] = load [[SELF_ADDR]]
  // CHECK-NOT:     [[SELF_ADDR]]
  // CHECK-NOT:     [[SELF1]]
  // CHECK:         [[SUPER1:%.*]] = upcast [[SELF1]]
  // CHECK-NOT:     [[SELF_ADDR]]
  // CHECK-NOT:     [[SELF1]]
  // CHECK-NOT:     [[SUPER1]]
  // CHECK:         [[SUPER2:%.*]] = apply {{.*}}([[SUPER1]])
  // CHECK-NEXT:     null_class
  // CHECK-NEXT:     store
  // CHECK-NEXT:    [[SELF2:%.*]] = unchecked_ref_cast [[SUPER2]]
  // CHECK-NEXT:    store [[SELF2]] to [[SELF_ADDR]]
  // CHECK-NOT:     [[SELF_ADDR]]
  // CHECK-NOT:     [[SELF1]]
  // CHECK-NOT:     [[SUPER1]]
  // CHECK-NOT:     [[SELF2]]
  // CHECK-NOT:     [[SUPER2]]
  // CHECK:         [[SELF_FINAL:%.*]] = load [[SELF_ADDR]]
  // CHECK-NEXT:    retain{{.*}} [[SELF_FINAL]]
  // CHECK-NEXT:    release{{.*}} [[SELF_BOX]]
  // CHECK-NEXT:    return [[SELF_FINAL]]
  required init() {
    super.init()
  }

  // CHECK-LABEL: sil shared [transparent] @_TTDFC15guaranteed_self1D3foofS0_FSiT_ : $@convention(method) (Int, @guaranteed D) -> ()
  // CHECK:       bb0({{.*}} [[SELF:%.*]]):
  // CHECK:         retain{{.*}} [[SELF]]
  // CHECK:         release{{.*}} [[SELF]]
  // CHECK-NOT:     release{{.*}} [[SELF]]
  // CHECK:       }
  dynamic override func foo(x: Int) {
    self.foo(x)
  }
}

func S_curryThunk(s: S) -> (S -> Int -> ()/*, Int -> ()*/) {
  return (S.foo /*, s.foo*/)
}

func AO_curryThunk<T>(ao: AO<T>) -> (AO<T> -> Int -> ()/*, Int -> ()*/) {
  return (AO.foo /*, ao.foo*/)
}

// ----------------------------------------------------------------------------
// Make sure that we properly translate in_guaranteed parameters
// correctly if we are asked to.
// ----------------------------------------------------------------------------

// CHECK-LABEL: sil [transparent] [thunk] @_TTWV15guaranteed_self9FakeArrayS_12SequenceTypeS_FS1_17_constrainElementuRq_S1__fq_Fqq_S_20SequenceDefaultsType7ElementT_ : $@convention(witness_method) (@in FakeElement, @in_guaranteed FakeArray) -> () {
// CHECK: bb0([[ARG0_PTR:%.*]] : $*FakeElement, [[ARG1_PTR:%.*]] : $*FakeArray):
// CHECK: [[GUARANTEED_COPY_STACK_SLOT:%.*]] = alloc_stack $FakeArray
// CHECK: copy_addr [[ARG1_PTR]] to [initialization] [[GUARANTEED_COPY_STACK_SLOT]]#1
// CHECK: [[ARG0:%.*]] = load [[ARG0_PTR]]
// CHECK: [[GUARANTEED_COPY:%.*]] = load [[GUARANTEED_COPY_STACK_SLOT]]#1
// CHECK: [[TRANSLATION_STACK_SLOT:%.*]] = alloc_stack $FakeArray
// CHECK: store [[GUARANTEED_COPY]] to [[TRANSLATION_STACK_SLOT:%.*]]#1
// CHECK: function_ref guaranteed_self.SequenceDefaultsType._constrainElement : <A where A: guaranteed_self.SequenceDefaultsType> (A)(guaranteed_self.FakeElement) -> ()
// CHECK: [[FUN:%.*]] = function_ref @_{{.*}}
// CHECK: apply [[FUN]]<FakeArray, FakeElement, FakeGenerator, FakeElement>([[ARG0]], [[TRANSLATION_STACK_SLOT]]#1)
// CHECK: destroy_addr [[TRANSLATION_STACK_SLOT]]#1

class Z {}

public struct FakeGenerator {}
public struct FakeArray {
  var z = Z()
}
public struct FakeElement {}

public protocol FakeGeneratorType {
  typealias Element
}

extension FakeGenerator : FakeGeneratorType {
  public typealias Element = FakeElement
}

public protocol SequenceDefaultsType {
  typealias Element
  typealias Generator : FakeGeneratorType
}

extension SequenceDefaultsType {
  public final func _constrainElement(FakeGenerator.Element) {}
}

public protocol SequenceType : SequenceDefaultsType {
  func _constrainElement(Element)
}


extension FakeArray : SequenceType {
  typealias Element = FakeElement
  typealias Generator = FakeGenerator

  func _containsElement(Element) {}
}

// -----------------------------------------------------------------------------
// Make sure that we properly add retains when emitting code for curried
// functions.
// -----------------------------------------------------------------------------

class Kraken {
  func enrage() {}
}

class CurriedTestBar {
  func bar(x: Kraken)(_ y: Kraken)(_ z: Kraken) -> Kraken {
    return z
  }
}

// Make sure we create a closure and pass it in @owned with a retain before it.
//
// CHECK-LABEL: sil hidden @_TF15guaranteed_self13curried_test0FT_T_ : $@convention(thin) () -> () {
// CHECK: [[FUNC:%.*]] = function_ref @_TFC15guaranteed_self14CurriedTestBarCfMS0_FT_S0_
// CHECK: [[CTB:%.*]] = apply [[FUNC]](
// CHECK: [[THUNK_CONSTRUCTOR:%.*]] = function_ref @_TFC15guaranteed_self14CurriedTestBar3barFS0_FCS_6KrakenFS1_FS1_S1_ : $@convention(thin) (@owned CurriedTestBar) -> @owned @callee_owned (@owned Kraken) -> @owned @callee_owned (@owned Kraken) -> @owned @callee_owned (@owned Kraken) -> @owned Kraken
// CHECK: strong_retain [[CTB]]
// CHECK: [[CLOSURE:%.*]] = apply [[THUNK_CONSTRUCTOR]]([[CTB]]
// CHECK-NOT: strong_release [[CTB]]
// CHECK: strong_release [[CLOSURE]]
// CHECK-NEXT: strong_release [[CTB]]
// CHECK-NOT: strong_release
// CHECK: return
func curried_test0() {
  let b = CurriedTestBar()
  let bar1 = b.bar
}

// CHECK-LABEL: sil hidden @_TF15guaranteed_self13curried_test1FT_T_ : $@convention(thin) () -> () {
// CHECK: [[KRAKEN_CONSTRUCTOR:%.*]] = function_ref @_TFC15guaranteed_self6KrakenCfMS0_FT_S0_ : $@convention(thin) (@thick Kraken.Type) -> @owned Kraken
// CHECK: [[KRAKEN:%.*]] = apply [[KRAKEN_CONSTRUCTOR]](
// CHECK: [[CTB_CONSTRUCTOR:%.*]] = function_ref @_TFC15guaranteed_self14CurriedTestBarCfMS0_FT_S0_
// CHECK: [[CTB:%.*]] = apply [[CTB_CONSTRUCTOR]](
// CHECK: [[THUNK_CONSTRUCTOR:%.*]] = function_ref @_TFC15guaranteed_self14CurriedTestBar3barfS0_FCS_6KrakenFS1_FS1_S1_ : $@convention(thin) (@owned Kraken, @owned CurriedTestBar) -> @owned @callee_owned (@owned Kraken) -> @owned @callee_owned (@owned Kraken) -> @owned Kraken
// CHECK: strong_retain [[CTB]]
// CHECK-NEXT: strong_retain [[KRAKEN]]
// CHECK-NEXT: [[CLOSURE:%.*]] = apply [[THUNK_CONSTRUCTOR]]([[KRAKEN]], [[CTB]])
// CHECK-NEXT: debug_value
// CHECK-NEXT: strong_release [[CLOSURE]]
// CHECK-NEXT: strong_release [[CTB]]
// CHECK-NEXT: strong_release [[KRAKEN]]
// CHECK-NEXT: tuple
// CHECK-NEXT: return
func curried_test1() {
  let k = Kraken()
  let b = CurriedTestBar()
  let bar2 = b.bar(k)
}

// CHECK-LABEL: sil hidden @_TF15guaranteed_self13curried_test2FT_T_ : $@convention(thin) () -> () {
// CHECK: [[KRAKEN_CONSTRUCTOR:%.*]] = function_ref @_TFC15guaranteed_self6KrakenCfMS0_FT_S0_ : $@convention(thin) (@thick Kraken.Type) -> @owned Kraken
// CHECK: [[KRAKEN:%.*]] = apply [[KRAKEN_CONSTRUCTOR]](
// CHECK: [[CTB_CONSTRUCTOR:%.*]] = function_ref @_TFC15guaranteed_self14CurriedTestBarCfMS0_FT_S0_
// CHECK: [[CTB:%.*]] = apply [[CTB_CONSTRUCTOR]](
// CHECK: [[THUNK_CONSTRUCTOR:%.*]] = function_ref @_TFC15guaranteed_self14CurriedTestBar3barfS0_fCS_6KrakenFS1_FS1_S1_ : $@convention(thin) (@owned Kraken, @owned Kraken, @owned CurriedTestBar) -> @owned @callee_owned (@owned Kraken) -> @owned Kraken
// CHECK: strong_retain [[CTB]]
// CHECK-NEXT: strong_retain [[KRAKEN]]
// CHECK-NEXT: strong_retain [[KRAKEN]]
// CHECK-NEXT: [[CLOSURE:%.*]] = apply [[THUNK_CONSTRUCTOR]]([[KRAKEN]], [[KRAKEN]], [[CTB]])
// CHECK-NEXT: debug_value
// CHECK-NEXT: strong_release [[CLOSURE]]
// CHECK-NEXT: strong_release [[CTB]]
// CHECK-NEXT: strong_release [[KRAKEN]]
// CHECK-NEXT: tuple
// CHECK-NEXT: return
func curried_test2() {
  let k = Kraken()
  let b = CurriedTestBar()
  let bar3 = b.bar(k)(k)
}

// CHECK-LABEL: sil hidden @_TF15guaranteed_self13curried_test3FT_T_ : $@convention(thin) () -> () {
// CHECK: [[KRAKEN_CONSTRUCTOR:%.*]] = function_ref @_TFC15guaranteed_self6KrakenCfMS0_FT_S0_ : $@convention(thin) (@thick Kraken.Type) -> @owned Kraken
// CHECK: [[KRAKEN:%.*]] = apply [[KRAKEN_CONSTRUCTOR]](
// CHECK: [[CTB_CONSTRUCTOR:%.*]] = function_ref @_TFC15guaranteed_self14CurriedTestBarCfMS0_FT_S0_
// CHECK: [[CTB:%.*]] = apply [[CTB_CONSTRUCTOR]](
// CHECK: [[CLASS_METHOD:%.*]] = class_method [[CTB]] : $CurriedTestBar, #CurriedTestBar.bar!3 : CurriedTestBar -> (Kraken) -> (Kraken) -> (Kraken) -> Kraken , $@convention(method) (@owned Kraken, @owned Kraken, @owned Kraken, @guaranteed CurriedTestBar) -> @owned Kraken
// CHECK-NOT: strong_retain [[CTB]]
// CHECK: strong_retain [[KRAKEN]]
// CHECK-NEXT: strong_retain [[KRAKEN]]
// CHECK-NEXT: strong_retain [[KRAKEN]]
// CHECK-NEXT: [[NEW_KRAKEN:%.*]] = apply [[CLASS_METHOD]]([[KRAKEN]], [[KRAKEN]], [[KRAKEN]], [[CTB]])
// CHECK-NEXT: debug_value
// CHECK-NEXT: strong_release [[NEW_KRAKEN]]
// CHECK-NEXT: strong_release [[CTB]]
// CHECK-NEXT: strong_release [[KRAKEN]]
// CHECK-NEXT: tuple
// CHECK-NEXT: return
func curried_test3() {
  let k = Kraken()
  let b = CurriedTestBar()
  let bar4 = b.bar(k)(k)(k)
}

// -----------------------------------------------------------------------------
// Make sure that we do not emit extra retains when accessing let fields of
// guaranteed parameters.
// -----------------------------------------------------------------------------

func destroyShip(k: Kraken) {}

class LetFieldClass {
  let letk = Kraken()
  var vark = Kraken()

  // CHECK-LABEL: sil hidden @_TFC15guaranteed_self13LetFieldClass10letkMethodfS0_FT_T_ : $@convention(method) (@guaranteed LetFieldClass) -> () {
  // CHECK: bb0([[CLS:%.*]] : $LetFieldClass):
  // CHECK: [[KRAKEN_ADDR:%.*]] = ref_element_addr [[CLS]] : $LetFieldClass, #LetFieldClass.letk
  // CHECK-NEXT: [[KRAKEN:%.*]] = load [[KRAKEN_ADDR]]
  // CHECK-NEXT: [[KRAKEN_METH:%.*]] = class_method [[KRAKEN]]
  // CHECK-NEXT: apply [[KRAKEN_METH]]([[KRAKEN]])
  // CHECK-NEXT: [[KRAKEN_ADDR:%.*]] = ref_element_addr [[CLS]] : $LetFieldClass, #LetFieldClass.letk
  // CHECK-NEXT: [[KRAKEN:%.*]] = load [[KRAKEN_ADDR]]
  // CHECK-NEXT: strong_retain [[KRAKEN]]
  // CHECK: [[DESTROY_SHIP_FUN:%.*]] = function_ref @_TF15guaranteed_self11destroyShipFCS_6KrakenT_ : $@convention(thin) (@owned Kraken) -> ()
  // CHECK-NEXT: strong_retain [[KRAKEN]]
  // CHECK-NEXT: apply [[DESTROY_SHIP_FUN]]([[KRAKEN]])
  // CHECK-NEXT: [[KRAKEN_BOX:%.*]] = alloc_box $Kraken
  // CHECK-NEXT: [[KRAKEN_ADDR:%.*]] = ref_element_addr [[CLS]] : $LetFieldClass, #LetFieldClass.letk
  // CHECK-NEXT: [[KRAKEN2:%.*]] = load [[KRAKEN_ADDR]]
  // CHECK-NEXT: strong_retain [[KRAKEN2]]
  // CHECK-NEXT: store [[KRAKEN2]] to [[KRAKEN_BOX]]#1
  // CHECK: [[DESTROY_SHIP_FUN:%.*]] = function_ref @_TF15guaranteed_self11destroyShipFCS_6KrakenT_ : $@convention(thin) (@owned Kraken) -> ()
  // CHECK-NEXT: [[KRAKEN_COPY:%.*]] = load [[KRAKEN_BOX]]#1
  // CHECK-NEXT: strong_retain [[KRAKEN_COPY]]
  // CHECK-NEXT: apply [[DESTROY_SHIP_FUN]]([[KRAKEN_COPY]])
  // CHECK-NEXT: strong_release [[KRAKEN_BOX]]#0
  // CHECK-NEXT: strong_release [[KRAKEN]]
  // CHECK-NEXT: tuple
  // CHECK-NEXT: return
  func letkMethod() {
    letk.enrage()
    let ll = letk
    destroyShip(ll)
    var lv = letk
    destroyShip(lv)
  }

  // CHECK-LABEL: sil hidden @_TFC15guaranteed_self13LetFieldClass10varkMethodfS0_FT_T_ : $@convention(method) (@guaranteed LetFieldClass) -> () {
  // CHECK: bb0([[CLS:%.*]] : $LetFieldClass):
  // CHECK: [[KRAKEN_GETTER_FUN:%.*]] = class_method [[CLS]] : $LetFieldClass, #LetFieldClass.vark!getter.1 : LetFieldClass -> () -> Kraken , $@convention(method) (@guaranteed LetFieldClass) -> @owned Kraken
  // CHECK-NEXT: [[KRAKEN:%.*]] = apply [[KRAKEN_GETTER_FUN]]([[CLS]])
  // CHECK-NEXT: [[KRAKEN_METH:%.*]] = class_method [[KRAKEN]]
  // CHECK-NEXT: apply [[KRAKEN_METH]]([[KRAKEN]])
  // CHECK-NEXT: strong_release [[KRAKEN]]
  // CHECK-NEXT: [[KRAKEN_GETTER_FUN:%.*]] = class_method [[CLS]] : $LetFieldClass, #LetFieldClass.vark!getter.1 : LetFieldClass -> () -> Kraken , $@convention(method) (@guaranteed LetFieldClass) -> @owned Kraken
  // CHECK-NEXT: [[KRAKEN:%.*]] = apply [[KRAKEN_GETTER_FUN]]([[CLS]])
  // CHECK: [[DESTROY_SHIP_FUN:%.*]] = function_ref @_TF15guaranteed_self11destroyShipFCS_6KrakenT_ : $@convention(thin) (@owned Kraken) -> ()
  // CHECK-NEXT: strong_retain [[KRAKEN]]
  // CHECK-NEXT: apply [[DESTROY_SHIP_FUN]]([[KRAKEN]])
  // CHECK-NEXT: [[KRAKEN_BOX:%.*]] = alloc_box $Kraken
  // CHECK-NEXT: [[KRAKEN_GETTER_FUN:%.*]] = class_method [[CLS]] : $LetFieldClass, #LetFieldClass.vark!getter.1 : LetFieldClass -> () -> Kraken , $@convention(method) (@guaranteed LetFieldClass) -> @owned Kraken
  // CHECK-NEXT: [[KRAKEN2:%.*]] = apply [[KRAKEN_GETTER_FUN]]([[CLS]])
  // CHECK-NEXT: store [[KRAKEN2]] to [[KRAKEN_BOX]]#1
  // CHECK: [[DESTROY_SHIP_FUN:%.*]] = function_ref @_TF15guaranteed_self11destroyShipFCS_6KrakenT_ : $@convention(thin) (@owned Kraken) -> ()
  // CHECK-NEXT: [[KRAKEN_COPY:%.*]] = load [[KRAKEN_BOX]]#1
  // CHECK-NEXT: strong_retain [[KRAKEN_COPY]]
  // CHECK-NEXT: apply [[DESTROY_SHIP_FUN]]([[KRAKEN_COPY]])
  // CHECK-NEXT: strong_release [[KRAKEN_BOX]]#0
  // CHECK-NEXT: strong_release [[KRAKEN]]
  // CHECK-NEXT: tuple
  // CHECK-NEXT: return
  func varkMethod() {
    vark.enrage()
    let vl = vark
    destroyShip(vl)
    var vv = vark
    destroyShip(vv)
  }
}

// -----------------------------------------------------------------------------
// Make sure that in all of the following cases find has only one retain in it.
// -----------------------------------------------------------------------------

class ClassIntTreeNode {
  let value : Int
  let left, right : ClassIntTreeNode

  init() {}

  // CHECK-LABEL: sil hidden @_TFC15guaranteed_self16ClassIntTreeNode4findfS0_FSiS0_ : $@convention(method) (Int, @guaranteed ClassIntTreeNode) -> @owned ClassIntTreeNode {
  // CHECK-NOT: strong_release
  // CHECK: strong_retain
  // CHECK-NOT: strong_retain
  // CHECK-NOT: strong_release
  // CHECK: return
  func find(v : Int) -> ClassIntTreeNode {
    if v == value { return self }
    if v < value { return left.find(v) }
    return right.find(v)
  }
}
