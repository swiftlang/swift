
// RUN: %target-swift-frontend -module-name guaranteed_self -Xllvm -sil-full-demangle -emit-silgen %s -disable-objc-attr-requires-foundation-module -enable-sil-ownership | %FileCheck %s

protocol Fooable {
  init()
  func foo(_ x: Int)
  mutating func bar()
  mutating func bas()

  var prop1: Int { get set }
  var prop2: Int { get set }
  var prop3: Int { get nonmutating set }
}

protocol Barrable: class {
  init()
  func foo(_ x: Int)
  func bar()
  func bas()

  var prop1: Int { get set }
  var prop2: Int { get set }
  var prop3: Int { get set }
}

struct S: Fooable {
  var x: C? // Make the type nontrivial, so +0/+1 is observable.

  // CHECK-LABEL: sil hidden @$S15guaranteed_self1SV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thin S.Type) -> @owned S
  init() {}
  // TODO: Way too many redundant r/r pairs here. Should use +0 rvalues.
  // CHECK-LABEL: sil hidden @$S15guaranteed_self1SV3foo{{[_0-9a-zA-Z]*}}F : $@convention(method) (Int, @guaranteed S) -> () {
  // CHECK:       bb0({{.*}} [[SELF:%.*]] : @guaranteed $S):
  // CHECK-NOT:     copy_value [[SELF]]
  // CHECK-NOT:     destroy_value [[SELF]]
  func foo(_ x: Int) {
    self.foo(x)
  }

  func foooo(_ x: (Int, Bool)) {
    self.foooo(x)
  }

  // CHECK-LABEL: sil hidden @$S15guaranteed_self1SV3bar{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout S) -> ()
  // CHECK:       bb0([[SELF:%.*]] : @trivial $*S):
  // CHECK-NOT:     destroy_addr [[SELF]]
  mutating func bar() {
    self.bar()
  }
  // CHECK-LABEL: sil hidden @$S15guaranteed_self1SV3bas{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed S) -> ()
  // CHECK:       bb0([[SELF:%.*]] : @guaranteed $S):
  // CHECK-NOT:     copy_value [[SELF]]
  // CHECK-NOT:     destroy_value [[SELF]]
  func bas() {
    self.bas()
  }

  var prop1: Int = 0

  // Getter for prop1
  // CHECK-LABEL: sil hidden [transparent] @$S15guaranteed_self1SV5prop1Sivg : $@convention(method) (@guaranteed S) -> Int
  // CHECK:       bb0([[SELF:%.*]] : @guaranteed $S):
  // CHECK-NOT:     destroy_value [[SELF]]

  // Setter for prop1
  // CHECK-LABEL: sil hidden [transparent] @$S15guaranteed_self1SV5prop1Sivs : $@convention(method) (Int, @inout S) -> ()
  // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*S):
  // CHECK-NOT:     load [[SELF_ADDR]]
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]

  // materializeForSet for prop1
  // CHECK-LABEL: sil hidden [transparent] @$S15guaranteed_self1SV5prop1Sivm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*S):
  // CHECK-NOT:     load [[SELF_ADDR]]
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]

  var prop2: Int {
    // CHECK-LABEL: sil hidden @$S15guaranteed_self1SV5prop2Sivg : $@convention(method) (@guaranteed S) -> Int
    // CHECK:       bb0([[SELF:%.*]] : @guaranteed $S):
    // CHECK-NOT:     destroy_value [[SELF]]
    get { return 0 }
    // CHECK-LABEL: sil hidden @$S15guaranteed_self1SV5prop2Sivs : $@convention(method) (Int, @inout S) -> ()
    // CHECK-LABEL: sil hidden [transparent] @$S15guaranteed_self1SV5prop2Sivm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
    set { }
  }

  var prop3: Int {
    // CHECK-LABEL: sil hidden @$S15guaranteed_self1SV5prop3Sivg : $@convention(method) (@guaranteed S) -> Int
    // CHECK:       bb0([[SELF:%.*]] : @guaranteed $S):
    // CHECK-NOT:     destroy_value [[SELF]]
    get { return 0 }
    // CHECK-LABEL: sil hidden @$S15guaranteed_self1SV5prop3Sivs : $@convention(method) (Int, @guaranteed S) -> ()
    // CHECK:       bb0({{.*}} [[SELF:%.*]] : @guaranteed $S):
    // CHECK-NOT:     destroy_value [[SELF]]
    // CHECK-LABEL: sil hidden [transparent] @$S15guaranteed_self1SV5prop3Sivm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed S) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
    // CHECK:       bb0({{.*}} [[SELF:%.*]] : @guaranteed $S):
    // CHECK-NOT:     destroy_value [[SELF]]
    nonmutating set { }
  }
}

// Witness thunk for nonmutating 'foo'
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP3foo{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: Fooable) (Int, @in_guaranteed S) -> () {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK:         [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-NOT:     destroy_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for mutating 'bar'
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP3bar{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: Fooable) (@inout S) -> () {
// CHECK:       bb0([[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK-NOT:     load [[SELF_ADDR]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for 'bas', which is mutating in the protocol, but nonmutating
// in the implementation
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP3bas{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: Fooable) (@inout S) -> ()
// CHECK:       bb0([[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK:         [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK:         end_borrow [[SELF]]
// CHECK-NOT:     destroy_value [[SELF]]

// Witness thunk for prop1 getter
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP5prop1SivgTW : $@convention(witness_method: Fooable) (@in_guaranteed S) -> Int
// CHECK:       bb0([[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK:         [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-NOT:     destroy_value [[SELF]]
// CHECK-NOT:     destroy_value [[SELF]]

// Witness thunk for prop1 setter
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP5prop1SivsTW : $@convention(witness_method: Fooable) (Int, @inout S) -> () {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop1 materializeForSet
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP5prop1SivmTW : $@convention(witness_method: Fooable) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop2 getter
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP5prop2SivgTW : $@convention(witness_method: Fooable) (@in_guaranteed S) -> Int
// CHECK:       bb0([[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK:         [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-NOT:     destroy_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop2 setter
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP5prop2SivsTW : $@convention(witness_method: Fooable) (Int, @inout S) -> () {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop2 materializeForSet
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP5prop2SivmTW : $@convention(witness_method: Fooable) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout S) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop3 getter
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP5prop3SivgTW : $@convention(witness_method: Fooable) (@in_guaranteed S) -> Int
// CHECK:       bb0([[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK:         [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-NOT:     destroy_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop3 nonmutating setter
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP5prop3SivsTW : $@convention(witness_method: Fooable) (Int, @in_guaranteed S) -> ()
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK:         [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-NOT:     destroy_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness thunk for prop3 nonmutating materializeForSet
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self1SVAA7FooableA2aDP5prop3SivmTW : $@convention(witness_method: Fooable) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @in_guaranteed S) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*S):
// CHECK:         [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-NOT:     destroy_value [[SELF]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]
// CHECK:       } // end sil function '$S15guaranteed_self1SVAA7FooableA2aDP5prop3SivmTW'

//
// TODO: Expected output for the other cases
//

struct AO<T>: Fooable {
  var x: T?

  init() {}
  // CHECK-LABEL: sil hidden @$S15guaranteed_self2AOV3foo{{[_0-9a-zA-Z]*}}F : $@convention(method) <T> (Int, @in_guaranteed AO<T>) -> ()
  // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*AO<T>):
  // CHECK:         apply {{.*}} [[SELF_ADDR]]
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
  // CHECK:       }
  func foo(_ x: Int) {
    self.foo(x)
  }
  mutating func bar() {
    self.bar()
  }
  // CHECK-LABEL: sil hidden @$S15guaranteed_self2AOV3bas{{[_0-9a-zA-Z]*}}F : $@convention(method) <T> (@in_guaranteed AO<T>) -> ()
  // CHECK:       bb0([[SELF_ADDR:%.*]] : @trivial $*AO<T>):
  // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
  func bas() {
    self.bas()
  }


  var prop1: Int = 0
  var prop2: Int {
    // CHECK-LABEL: sil hidden @$S15guaranteed_self2AOV5prop2Sivg : $@convention(method) <T> (@in_guaranteed AO<T>) -> Int {
    // CHECK:       bb0([[SELF_ADDR:%.*]] : @trivial $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    get { return 0 }
    set { }
  }
  var prop3: Int {
    // CHECK-LABEL: sil hidden @$S15guaranteed_self2AOV5prop3Sivg : $@convention(method) <T> (@in_guaranteed AO<T>) -> Int
    // CHECK:       bb0([[SELF_ADDR:%.*]] : @trivial $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    get { return 0 }
    // CHECK-LABEL: sil hidden @$S15guaranteed_self2AOV5prop3Sivs : $@convention(method) <T> (Int, @in_guaranteed AO<T>) -> ()
    // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    // CHECK-LABEL: sil hidden [transparent] @$S15guaranteed_self2AOV5prop3Sivm : $@convention(method) <T> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @in_guaranteed AO<T>) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
    // CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*AO<T>):
    // CHECK-NOT:     destroy_addr [[SELF_ADDR]]
    // CHECK:       }
    nonmutating set { }
  }
}

// Witness for nonmutating 'foo'
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self2AOVyxGAA7FooableA2aEP3foo{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: Fooable) <τ_0_0> (Int, @in_guaranteed AO<τ_0_0>) -> ()
// CHECK:       bb0({{.*}} [[SELF_ADDR:%.*]] : @trivial $*AO<τ_0_0>):
// CHECK:         apply {{.*}} [[SELF_ADDR]]
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

// Witness for 'bar', which is mutating in protocol but nonmutating in impl
// CHECK-LABEL: sil private [transparent] [thunk] @$S15guaranteed_self2AOVyxGAA7FooableA2aEP3bar{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: Fooable) <τ_0_0> (@inout AO<τ_0_0>) -> ()
// CHECK:       bb0([[SELF_ADDR:%.*]] : @trivial $*AO<τ_0_0>):
// -- NB: This copy is not necessary, since we're willing to assume an inout
//        parameter is not mutably aliased.
// CHECK:         apply {{.*}}([[SELF_ADDR]])
// CHECK-NOT:     destroy_addr [[SELF_ADDR]]

class C: Fooable, Barrable {
  // Allocating initializer
  // CHECK-LABEL: sil hidden @$S15guaranteed_self1CC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick C.Type) -> @owned C
  // CHECK:         [[SELF1:%.*]] = alloc_ref $C
  // CHECK-NOT:     [[SELF1]]
  // CHECK:         [[SELF2:%.*]] = apply {{.*}}([[SELF1]])
  // CHECK-NOT:     [[SELF2]]
  // CHECK:         return [[SELF2]]

  // Initializing constructors still have the +1 in, +1 out convention.
  // CHECK-LABEL: sil hidden @$S15guaranteed_self1CC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned C) -> @owned C {
  // CHECK:       bb0([[SELF:%.*]] : @owned $C):
  // CHECK:         [[MARKED_SELF:%.*]] = mark_uninitialized [rootself] [[SELF]]
  // CHECK:         [[MARKED_SELF_RESULT:%.*]] = copy_value [[MARKED_SELF]]
  // CHECK:         destroy_value [[MARKED_SELF]]
  // CHECK:         return [[MARKED_SELF_RESULT]]
  // CHECK:       } // end sil function '$S15guaranteed_self1CC{{[_0-9a-zA-Z]*}}fc'

  // @objc thunk for initializing constructor
  // CHECK-LABEL: sil hidden [thunk] @$S15guaranteed_self1CC{{[_0-9a-zA-Z]*}}fcTo : $@convention(objc_method) (@owned C) -> @owned C
  // CHECK:       bb0([[SELF:%.*]] : @owned $C):
  // CHECK-NOT:     copy_value [[SELF]]
  // CHECK:         [[SELF2:%.*]] = apply {{%.*}}([[SELF]])
  // CHECK-NOT:     destroy_value [[SELF]]
  // CHECK-NOT:     destroy_value [[SELF2]]
  // CHECK:         return [[SELF2]]
  // CHECK: } // end sil function '$S15guaranteed_self1CC{{[_0-9a-zA-Z]*}}fcTo'
  @objc required init() {}


  // CHECK-LABEL: sil hidden @$S15guaranteed_self1CC3foo{{[_0-9a-zA-Z]*}}F : $@convention(method) (Int, @guaranteed C) -> ()
  // CHECK:       bb0({{.*}} [[SELF:%.*]] : @guaranteed $C):
  // CHECK-NOT:     copy_value
  // CHECK-NOT:     destroy_value
  // CHECK:       } // end sil function '$S15guaranteed_self1CC3foo{{[_0-9a-zA-Z]*}}F'

  // CHECK-LABEL: sil hidden [thunk] @$S15guaranteed_self1CC3foo{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (Int, C) -> () {
  // CHECK:       bb0({{.*}} [[SELF:%.*]] : @unowned $C):
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         apply {{.*}}({{.*}}, [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK:         destroy_value [[SELF_COPY]]
  // CHECK-NOT:     destroy_value [[SELF_COPY]]
  // CHECK-NOT:     destroy_value [[SELF]]
  // CHECK:       } // end sil function '$S15guaranteed_self1CC3foo{{[_0-9a-zA-Z]*}}FTo'
  @objc func foo(_ x: Int) {
    self.foo(x)
  }
  @objc func bar() {
    self.bar()
  }
  @objc func bas() {
    self.bas()
  }

  // CHECK-LABEL: sil hidden [transparent] [thunk] @$S15guaranteed_self1CC5prop1SivgTo : $@convention(objc_method) (C) -> Int
  // CHECK:       bb0([[SELF:%.*]] : @unowned $C):
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         apply {{.*}}([[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK:         destroy_value [[SELF_COPY]]
  // CHECK-NOT:     destroy_value [[SELF]]
  // CHECK-NOT:     destroy_value [[SELF_COPY]]

  // CHECK-LABEL: sil hidden [transparent] [thunk] @$S15guaranteed_self1CC5prop1SivsTo : $@convention(objc_method) (Int, C) -> ()
  // CHECK:       bb0({{.*}} [[SELF:%.*]] : @unowned $C):
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         apply {{.*}} [[BORROWED_SELF_COPY]]
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK:         destroy_value [[SELF_COPY]]
  // CHECK-NOT:     destroy_value [[SELF_COPY]]
  // CHECK-NOT:     destroy_value [[SELF]]
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
  // CHECK-LABEL: sil hidden @$S15guaranteed_self1DC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick D.Type) -> @owned D
  // CHECK:         [[SELF1:%.*]] = alloc_ref $D
  // CHECK-NOT:     [[SELF1]]
  // CHECK:         [[SELF2:%.*]] = apply {{.*}}([[SELF1]])
  // CHECK-NOT:     [[SELF1]]
  // CHECK-NOT:     [[SELF2]]
  // CHECK:         return [[SELF2]]

  // CHECK-LABEL: sil hidden @$S15guaranteed_self1DC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned D) -> @owned D
  // CHECK:       bb0([[SELF:%.*]] : @owned $D):
  // CHECK:         [[SELF_BOX:%.*]] = alloc_box ${ var D }
  // CHECK-NEXT:    [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [derivedself] [[SELF_BOX]]
  // CHECK-NEXT:    [[PB:%.*]] = project_box [[MARKED_SELF_BOX]]
  // CHECK-NEXT:    store [[SELF]] to [init] [[PB]]
  // CHECK-NOT:     [[PB]]
  // CHECK:         [[SELF1:%.*]] = load [take] [[PB]]
  // CHECK-NEXT:    [[SUPER1:%.*]] = upcast [[SELF1]]
  // CHECK-NOT:     [[PB]]
  // CHECK:         [[SUPER2:%.*]] = apply {{.*}}([[SUPER1]])
  // CHECK-NEXT:    [[SELF2:%.*]] = unchecked_ref_cast [[SUPER2]]
  // CHECK-NEXT:    store [[SELF2]] to [init] [[PB]]
  // CHECK-NOT:     [[PB]]
  // CHECK-NOT:     [[SELF1]]
  // CHECK-NOT:     [[SUPER1]]
  // CHECK-NOT:     [[SELF2]]
  // CHECK-NOT:     [[SUPER2]]
  // CHECK:         [[SELF_FINAL:%.*]] = load [copy] [[PB]]
  // CHECK-NEXT:    destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT:    return [[SELF_FINAL]]
  required init() {
    super.init()
  }

  // CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S15guaranteed_self1DC3foo{{[_0-9a-zA-Z]*}}FTD : $@convention(method) (Int, @guaranteed D) -> ()
  // CHECK:       bb0({{.*}} [[SELF:%.*]] : @guaranteed $D):
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         destroy_value [[SELF_COPY]]
  // CHECK-NOT:     destroy_value [[SELF_COPY]]
  // CHECK-NOT:     destroy_value [[SELF]]
  // CHECK:       }
  dynamic override func foo(_ x: Int) {
    self.foo(x)
  }
}

func S_curryThunk(_ s: S) -> ((S) -> (Int) -> ()/*, Int -> ()*/) {
  return (S.foo /*, s.foo*/)
}

func AO_curryThunk<T>(_ ao: AO<T>) -> ((AO<T>) -> (Int) -> ()/*, Int -> ()*/) {
  return (AO.foo /*, ao.foo*/)
}

// ----------------------------------------------------------------------------
// Make sure that we properly translate in_guaranteed parameters
// correctly if we are asked to.
// ----------------------------------------------------------------------------


// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] @$S15guaranteed_self9FakeArrayVAA8SequenceA2aDP17_constrainElement{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: Sequence) (@in_guaranteed FakeElement, @in_guaranteed FakeArray) -> () {
// CHECK: bb0([[ARG0_PTR:%.*]] : @trivial $*FakeElement, [[ARG1_PTR:%.*]] : @trivial $*FakeArray):
// CHECK: [[ARG0:%.*]] = load [trivial] [[ARG0_PTR]]
// CHECK: function_ref (extension in guaranteed_self):guaranteed_self.SequenceDefaults._constrainElement
// CHECK: [[FUN:%.*]] = function_ref @{{.*}}
// CHECK: apply [[FUN]]<FakeArray>([[ARG0]], [[ARG1_PTR]])

class Z {}

public struct FakeGenerator {}
public struct FakeArray {
  var z = Z()
}
public struct FakeElement {}

public protocol FakeGeneratorProtocol {
  associatedtype Element
}

extension FakeGenerator : FakeGeneratorProtocol {
  public typealias Element = FakeElement
}

public protocol SequenceDefaults {
  associatedtype Element
  associatedtype Generator : FakeGeneratorProtocol
}

extension SequenceDefaults {
  public func _constrainElement(_: FakeGenerator.Element) {}
}

public protocol Sequence : SequenceDefaults {
  func _constrainElement(_: Element)
}


extension FakeArray : Sequence {
  public typealias Element = FakeElement
  public typealias Generator = FakeGenerator

  func _containsElement(_: Element) {}
}

// -----------------------------------------------------------------------------
// Make sure that we do not emit extra copy_values when accessing let fields of
// guaranteed parameters.
// -----------------------------------------------------------------------------

class Kraken {
  func enrage() {}
}

func destroyShip(_ k: Kraken) {}

class LetFieldClass {
  let letk = Kraken()
  var vark = Kraken()

  // CHECK-LABEL: sil hidden @$S15guaranteed_self13LetFieldClassC10letkMethod{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed LetFieldClass) -> () {
  // CHECK: bb0([[CLS:%.*]] : @guaranteed $LetFieldClass):
  // CHECK: [[KRAKEN_ADDR:%.*]] = ref_element_addr [[CLS]] : $LetFieldClass, #LetFieldClass.letk
  // CHECK-NEXT: [[KRAKEN:%.*]] = load_borrow [[KRAKEN_ADDR]]
  // CHECK-NEXT: [[KRAKEN_METH:%.*]] = class_method [[KRAKEN]]
  // CHECK-NEXT: apply [[KRAKEN_METH]]([[KRAKEN]])
  // CHECK: [[KRAKEN_ADDR:%.*]] = ref_element_addr [[CLS]] : $LetFieldClass, #LetFieldClass.letk
  // CHECK-NEXT: [[KRAKEN:%.*]] = load [copy] [[KRAKEN_ADDR]]
  // CHECK:      [[BORROWED_KRAKEN:%.*]] = begin_borrow [[KRAKEN]]
  // CHECK: [[DESTROY_SHIP_FUN:%.*]] = function_ref @$S15guaranteed_self11destroyShipyyAA6KrakenCF : $@convention(thin) (@guaranteed Kraken) -> ()
  // CHECK-NEXT: apply [[DESTROY_SHIP_FUN]]([[BORROWED_KRAKEN]])
  // CHECK-NEXT: end_borrow [[BORROWED_KRAKEN]] from [[KRAKEN]]
  // CHECK-NEXT: [[KRAKEN_BOX:%.*]] = alloc_box ${ var Kraken }
  // CHECK-NEXT: [[PB:%.*]] = project_box [[KRAKEN_BOX]]
  // CHECK-NEXT: [[KRAKEN_ADDR:%.*]] = ref_element_addr [[CLS]] : $LetFieldClass, #LetFieldClass.letk
  // CHECK-NEXT: [[KRAKEN2:%.*]] = load [copy] [[KRAKEN_ADDR]]
  // CHECK-NEXT: store [[KRAKEN2]] to [init] [[PB]]
  // CHECK-NEXT: [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*Kraken
  // CHECK-NEXT: [[KRAKEN_COPY:%.*]] = load [copy] [[READ]]
  // CHECK-NEXT: end_access [[READ]] : $*Kraken
  // CHECK-NEXT: [[BORROWED_KRAKEN_COPY:%.*]] = begin_borrow [[KRAKEN_COPY]]
  // CHECK: [[DESTROY_SHIP_FUN:%.*]] = function_ref @$S15guaranteed_self11destroyShipyyAA6KrakenCF : $@convention(thin) (@guaranteed Kraken) -> ()
  // CHECK-NEXT: apply [[DESTROY_SHIP_FUN]]([[BORROWED_KRAKEN_COPY]])
  // CHECK-NEXT: end_borrow [[BORROWED_KRAKEN_COPY]]
  // CHECK-NEXT: destroy_value [[KRAKEN_COPY]]
  // CHECK-NEXT: destroy_value [[KRAKEN_BOX]]
  // CHECK-NEXT: destroy_value [[KRAKEN]]
  // CHECK-NEXT: tuple
  // CHECK-NEXT: return
  func letkMethod() {
    letk.enrage()
    let ll = letk
    destroyShip(ll)
    var lv = letk
    destroyShip(lv)
  }

  // CHECK-LABEL: sil hidden @$S15guaranteed_self13LetFieldClassC10varkMethod{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed LetFieldClass) -> () {
  // CHECK: bb0([[CLS:%.*]] : @guaranteed $LetFieldClass):
  // CHECK: [[KRAKEN_GETTER_FUN:%.*]] = class_method [[CLS]] : $LetFieldClass, #LetFieldClass.vark!getter.1 : (LetFieldClass) -> () -> Kraken, $@convention(method) (@guaranteed LetFieldClass) -> @owned Kraken
  // CHECK-NEXT: [[KRAKEN:%.*]] = apply [[KRAKEN_GETTER_FUN]]([[CLS]])
  // CHECK-NEXT: [[BORROWED_KRAKEN:%.*]] = begin_borrow [[KRAKEN]]
  // CHECK-NEXT: [[KRAKEN_METH:%.*]] = class_method [[BORROWED_KRAKEN]]
  // CHECK-NEXT: apply [[KRAKEN_METH]]([[BORROWED_KRAKEN]])
  // CHECK-NEXT: end_borrow [[BORROWED_KRAKEN]] from [[KRAKEN]]
  // CHECK-NEXT: destroy_value [[KRAKEN]]
  // CHECK-NEXT: [[KRAKEN_GETTER_FUN:%.*]] = class_method [[CLS]] : $LetFieldClass, #LetFieldClass.vark!getter.1 : (LetFieldClass) -> () -> Kraken, $@convention(method) (@guaranteed LetFieldClass) -> @owned Kraken
  // CHECK-NEXT: [[KRAKEN:%.*]] = apply [[KRAKEN_GETTER_FUN]]([[CLS]])
  // CHECK:      [[BORROWED_KRAKEN:%.*]] = begin_borrow [[KRAKEN]]
  // CHECK: [[DESTROY_SHIP_FUN:%.*]] = function_ref @$S15guaranteed_self11destroyShipyyAA6KrakenCF : $@convention(thin) (@guaranteed Kraken) -> ()
  // CHECK-NEXT: apply [[DESTROY_SHIP_FUN]]([[BORROWED_KRAKEN]])
  // CHECK-NEXT: end_borrow [[BORROWED_KRAKEN]] from [[KRAKEN]]
  // CHECK-NEXT: [[KRAKEN_BOX:%.*]] = alloc_box ${ var Kraken }
  // CHECK-NEXT: [[PB:%.*]] = project_box [[KRAKEN_BOX]]
  // CHECK-NEXT: [[KRAKEN_GETTER_FUN:%.*]] = class_method [[CLS]] : $LetFieldClass, #LetFieldClass.vark!getter.1 : (LetFieldClass) -> () -> Kraken, $@convention(method) (@guaranteed LetFieldClass) -> @owned Kraken
  // CHECK-NEXT: [[KRAKEN2:%.*]] = apply [[KRAKEN_GETTER_FUN]]([[CLS]])
  // CHECK-NEXT: store [[KRAKEN2]] to [init] [[PB]]
  // CHECK-NEXT: [[WRITE:%.*]] = begin_access [read] [unknown] [[PB]] : $*Kraken
  // CHECK-NEXT: [[KRAKEN_COPY:%.*]] = load [copy] [[WRITE]]
  // CHECK-NEXT: end_access [[WRITE]] : $*Kraken
  // CHECK-NEXT: [[BORROWED_KRAKEN_COPY:%.*]] = begin_borrow [[KRAKEN_COPY]]
  // CHECK: [[DESTROY_SHIP_FUN:%.*]] = function_ref @$S15guaranteed_self11destroyShipyyAA6KrakenCF : $@convention(thin) (@guaranteed Kraken) -> ()
  // CHECK-NEXT: apply [[DESTROY_SHIP_FUN]]([[BORROWED_KRAKEN_COPY]])
  // CHECK-NEXT: end_borrow [[BORROWED_KRAKEN_COPY]]
  // CHECK-NEXT: destroy_value [[KRAKEN_COPY]]
  // CHECK-NEXT: destroy_value [[KRAKEN_BOX]]
  // CHECK-NEXT: destroy_value [[KRAKEN]]
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
// Make sure that in all of the following cases find has only one copy_value in it.
// -----------------------------------------------------------------------------

class ClassIntTreeNode {
  let value : Int
  let left, right : ClassIntTreeNode

  init() {}

  // CHECK-LABEL: sil hidden @$S15guaranteed_self16ClassIntTreeNodeC4find{{[_0-9a-zA-Z]*}}F : $@convention(method) (Int, @guaranteed ClassIntTreeNode) -> @owned ClassIntTreeNode {
  // CHECK-NOT: destroy_value
  // CHECK: copy_value
  // CHECK-NOT: copy_value
  // CHECK-NOT: destroy_value
  // CHECK: return
  func find(_ v : Int) -> ClassIntTreeNode {
    if v == value { return self }
    if v < value { return left.find(v) }
    return right.find(v)
  }
}
