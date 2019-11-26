// RUN: %target-swift-emit-silgen -parse-stdlib -module-name keypaths %s | %FileCheck %s

import Swift

struct S<T> {
  var x: T
  let y: String
  var z: C<T>

  var computed: C<T> { fatalError() }
  var observed: C<T> { didSet { fatalError() } }
  var reabstracted: () -> ()
}
class C<T> {
  final var x: T
  final let y: String
  final var z: S<T>

  var nonfinal: S<T>
  var computed: S<T> { fatalError() }
  var observed: S<T> { didSet { fatalError() } }
  final var reabstracted: () -> ()

  init() { fatalError() }
}

extension C {
  var `extension`: S<T> { fatalError() }
}

protocol P {
  var x: Int { get }
  var y: String { get set }
}

extension P {
  var z: String {
    return y
  }
  var w: String {
    get { return "" }
    nonmutating set { }
  }
}

struct T {
  var a: (Int, String)
  let b: (f: String, g: Int)
  let c: (x: C<Int>, y: C<String>)
}

/* TODO: When we support superclass requirements on protocols, we should test
 * this case as well.
protocol PoC : C<Int> {}
*/

// CHECK-LABEL: sil hidden [ossa] @{{.*}}storedProperties
func storedProperties<T>(_: T) {
  // CHECK: keypath $WritableKeyPath<S<T>, T>, <τ_0_0> (root $S<τ_0_0>; stored_property #S.x : $τ_0_0) <T>
  _ = \S<T>.x
  // CHECK: keypath $KeyPath<S<T>, String>, <τ_0_0> (root $S<τ_0_0>; stored_property #S.y : $String) <T>
  _ = \S<T>.y
  // CHECK: keypath $ReferenceWritableKeyPath<S<T>, T>, <τ_0_0> (root $S<τ_0_0>; stored_property #S.z : $C<τ_0_0>; stored_property #C.x : $τ_0_0) <T>
  _ = \S<T>.z.x
  // CHECK: keypath $ReferenceWritableKeyPath<C<T>, T>, <τ_0_0> (root $C<τ_0_0>; stored_property #C.x : $τ_0_0) <T>
  _ = \C<T>.x
  // CHECK: keypath $KeyPath<C<T>, String>, <τ_0_0> (root $C<τ_0_0>; stored_property #C.y : $String) <T>
  _ = \C<T>.y
  // CHECK: keypath $ReferenceWritableKeyPath<C<T>, T>, <τ_0_0> (root $C<τ_0_0>; stored_property #C.z : $S<τ_0_0>; stored_property #S.x : $τ_0_0) <T>
  _ = \C<T>.z.x
  // CHECK: keypath $KeyPath<C<T>, String>, <τ_0_0> (root $C<τ_0_0>; stored_property #C.z : $S<τ_0_0>; stored_property #S.z : $C<τ_0_0>; stored_property #C.y : $String) <T>
  _ = \C<T>.z.z.y
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}computedProperties
func computedProperties<T: P>(_: T) {
  // CHECK: keypath $ReferenceWritableKeyPath<C<T>, S<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $C<τ_0_0>;
  // CHECK-SAME: settable_property $S<τ_0_0>, 
  // CHECK-SAME:   id #C.nonfinal!getter.1 : <T> (C<T>) -> () -> S<T>,
  // CHECK-SAME:   getter @$s8keypaths1CC8nonfinalAA1SVyxGvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed C<τ_0_0>) -> @out S<τ_0_0>,
  // CHECK-SAME:   setter @$s8keypaths1CC8nonfinalAA1SVyxGvpAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed S<τ_0_0>, @in_guaranteed C<τ_0_0>) -> ()
  // CHECK-SAME: ) <T>
  _ = \C<T>.nonfinal

  // CHECK: keypath $KeyPath<C<T>, S<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $C<τ_0_0>;
  // CHECK-SAME: gettable_property $S<τ_0_0>,
  // CHECK-SAME:   id #C.computed!getter.1 : <T> (C<T>) -> () -> S<T>,
  // CHECK-SAME:   getter @$s8keypaths1CC8computedAA1SVyxGvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed C<τ_0_0>) -> @out S<τ_0_0>
  // CHECK-SAME: ) <T>
  _ = \C<T>.computed

  // CHECK: keypath $ReferenceWritableKeyPath<C<T>, S<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $C<τ_0_0>;
  // CHECK-SAME: settable_property $S<τ_0_0>, 
  // CHECK-SAME:   id #C.observed!getter.1 : <T> (C<T>) -> () -> S<T>,
  // CHECK-SAME:   getter @$s8keypaths1CC8observedAA1SVyxGvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed C<τ_0_0>) -> @out S<τ_0_0>,
  // CHECK-SAME:   setter @$s8keypaths1CC8observedAA1SVyxGvpAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed S<τ_0_0>, @in_guaranteed C<τ_0_0>) -> ()
  // CHECK-SAME: ) <T>
  _ = \C<T>.observed

  _ = \C<T>.nonfinal.x
  _ = \C<T>.computed.x
  _ = \C<T>.observed.x
  _ = \C<T>.z.computed
  _ = \C<T>.z.observed
  _ = \C<T>.observed.x

  // CHECK: keypath $ReferenceWritableKeyPath<C<T>, () -> ()>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $C<τ_0_0>;
  // CHECK-SAME: settable_property $() -> (), 
  // CHECK-SAME:   id ##C.reabstracted,
  // CHECK-SAME:   getter @$s8keypaths1CC12reabstractedyycvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed C<τ_0_0>) -> @out @callee_guaranteed () -> @out (),
  // CHECK-SAME:   setter @$s8keypaths1CC12reabstractedyycvpAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed @callee_guaranteed () -> @out (), @in_guaranteed C<τ_0_0>) -> ()
  // CHECK-SAME: ) <T>
  _ = \C<T>.reabstracted

  // CHECK: keypath $KeyPath<S<T>, C<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $S<τ_0_0>; gettable_property $C<τ_0_0>,
  // CHECK-SAME: id @$s8keypaths1SV8computedAA1CCyxGvg : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @owned C<τ_0_0>,
  // CHECK-SAME:   getter @$s8keypaths1SV8computedAA1CCyxGvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed S<τ_0_0>) -> @out C<τ_0_0>
  // CHECK-SAME: ) <T>
  _ = \S<T>.computed

  // CHECK: keypath $WritableKeyPath<S<T>, C<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $S<τ_0_0>;
  // CHECK-SAME: settable_property $C<τ_0_0>,
  // CHECK-SAME:   id @$s8keypaths1SV8observedAA1CCyxGvg : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @owned C<τ_0_0>,
  // CHECK-SAME:   getter @$s8keypaths1SV8observedAA1CCyxGvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed S<τ_0_0>) -> @out C<τ_0_0>,
  // CHECK-SAME:   setter @$s8keypaths1SV8observedAA1CCyxGvpAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed C<τ_0_0>, @inout S<τ_0_0>) -> ()
  // CHECK-SAME: ) <T>
  _ = \S<T>.observed
  _ = \S<T>.z.nonfinal
  _ = \S<T>.z.computed
  _ = \S<T>.z.observed
  _ = \S<T>.computed.x
  _ = \S<T>.computed.y
  // CHECK: keypath $WritableKeyPath<S<T>, () -> ()>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME:  root $S<τ_0_0>;
  // CHECK-SAME:  settable_property $() -> (),
  // CHECK-SAME:    id ##S.reabstracted,
  // CHECK-SAME:    getter @$s8keypaths1SV12reabstractedyycvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed S<τ_0_0>) -> @out @callee_guaranteed () -> @out (),
  // CHECK-SAME:    setter @$s8keypaths1SV12reabstractedyycvpAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed @callee_guaranteed () -> @out (), @inout S<τ_0_0>) -> ()
  // CHECK-SAME: ) <T>
  _ = \S<T>.reabstracted

  // CHECK: keypath $KeyPath<T, Int>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $τ_0_0;
  // CHECK-SAME: gettable_property $Int, 
  // CHECK-SAME:   id #P.x!getter.1 : <Self where Self : P> (Self) -> () -> Int,
  // CHECK-SAME:   getter @$s8keypaths1PP1xSivpAaBRzlxTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out Int
  // CHECK-SAME: ) <T>
  _ = \T.x
  // CHECK: keypath $WritableKeyPath<T, String>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $τ_0_0;
  // CHECK-SAME: settable_property $String,
  // CHECK-SAME:   id #P.y!getter.1 : <Self where Self : P> (Self) -> () -> String,
  // CHECK-SAME:   getter @$s8keypaths1PP1ySSvpAaBRzlxTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out String,
  // CHECK-SAME:   setter @$s8keypaths1PP1ySSvpAaBRzlxTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed String, @inout τ_0_0) -> ()
  // CHECK-SAME: ) <T>
  _ = \T.y

  // CHECK: keypath $KeyPath<T, String>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $τ_0_0;
  // CHECK-SAME: gettable_property $String,
  // CHECK-SAME:   id @$s8keypaths1PPAAE1zSSvg
  _ = \T.z
}

struct Concrete: P {
  var x: Int
  var y: String
}

// CHECK-LABEL: sil hidden [ossa] @$s8keypaths35keyPathsWithSpecificGenericInstanceyyF
func keyPathsWithSpecificGenericInstance() {
  // CHECK: keypath $KeyPath<Concrete, String>, (
  // CHECK-SAME: gettable_property $String,
  // CHECK-SAME:   id @$s8keypaths1PPAAE1zSSvg
  // CHECK-SAME:   getter @$s8keypaths1PPAAE1zSSvpAA8ConcreteVTK : $@convention(thin) (@in_guaranteed Concrete) -> @out String
  _ = \Concrete.z
  _ = \S<Concrete>.computed
}

class AA<T> {
  var a: Int { get { return 0 } set { } }
}
class BB<U, V>: AA<V> {
}

func keyPathForInheritedMember() {
  _ = \BB<Int, String>.a
}

func keyPathForExistentialMember() {
  _ = \P.x
  _ = \P.y
  _ = \P.z
  _ = \P.w
}

struct OptionalFields {
  var x: S<Int>?
}
struct OptionalFields2 {
  var y: OptionalFields?
}

// CHECK-LABEL: sil hidden [ossa] @$s8keypaths18keyPathForOptionalyyF
func keyPathForOptional() {
  // CHECK: keypath $WritableKeyPath<OptionalFields, S<Int>>, (
  // CHECK-SAME:   stored_property #OptionalFields.x : $Optional<S<Int>>;
  // CHECK-SAME:   optional_force : $S<Int>)
  _ = \OptionalFields.x!
  // CHECK: keypath $KeyPath<OptionalFields, Optional<String>>, (
  // CHECK-SAME:   stored_property #OptionalFields.x : $Optional<S<Int>>;
  // CHECK-SAME:   optional_chain : $S<Int>;
  // CHECK-SAME:   stored_property #S.y : $String;
  // CHECK-SAME:   optional_wrap : $Optional<String>)
  _ = \OptionalFields.x?.y
  // CHECK: keypath $KeyPath<OptionalFields2, Optional<S<Int>>>, (
  // CHECK-SAME:   root $OptionalFields2;
  // CHECK-SAME:   stored_property #OptionalFields2.y : $Optional<OptionalFields>;
  // CHECK-SAME:   optional_chain : $OptionalFields;
  // CHECK-SAME:   stored_property #OptionalFields.x : $Optional<S<Int>>)
  _ = \OptionalFields2.y?.x
}

class StorageQualified {
  weak var tooWeak: StorageQualified?
  unowned var disowned: StorageQualified
  
  init() { fatalError() }
}

final class FinalStorageQualified {
  weak var tooWeak: StorageQualified?
  unowned var disowned: StorageQualified
  
  init() { fatalError() }
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}keyPathForStorageQualified
func keyPathForStorageQualified() {
  // CHECK: = keypath $ReferenceWritableKeyPath<StorageQualified, Optional<StorageQualified>>,
  // CHECK-SAME: settable_property $Optional<StorageQualified>, id #StorageQualified.tooWeak!getter.1
  _ = \StorageQualified.tooWeak
  // CHECK: = keypath $ReferenceWritableKeyPath<StorageQualified, StorageQualified>,
  // CHECK-SAME: settable_property $StorageQualified, id #StorageQualified.disowned!getter.1
  _ = \StorageQualified.disowned

  // CHECK: = keypath $ReferenceWritableKeyPath<FinalStorageQualified, Optional<StorageQualified>>,
  // CHECK-SAME: settable_property $Optional<StorageQualified>, id ##FinalStorageQualified.tooWeak
  _ = \FinalStorageQualified.tooWeak
  // CHECK: = keypath $ReferenceWritableKeyPath<FinalStorageQualified, StorageQualified>,
  // CHECK-SAME: settable_property $StorageQualified, id ##FinalStorageQualified.disowned
  _ = \FinalStorageQualified.disowned
}

struct IUOProperty {
  var iuo: IUOBlob!
}

struct IUOBlob {
  var x: Int
  subscript(y: String) -> String {
    get { return y }
    set {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}11iuoKeyPaths
func iuoKeyPaths() {
  // CHECK: = keypath $WritableKeyPath<IUOProperty, Int>,
  // CHECK-SAME: stored_property #IUOProperty.iuo
  // CHECK-SAME: optional_force
  // CHECK-SAME: stored_property #IUOBlob.x
  _ = \IUOProperty.iuo.x
  // CHECK: = keypath $WritableKeyPath<IUOProperty, Int>,
  // CHECK-SAME: stored_property #IUOProperty.iuo
  // CHECK-SAME: optional_force
  // CHECK-SAME: stored_property #IUOBlob.x
  _ = \IUOProperty.iuo!.x
}

class Bass: Hashable {
  static func ==(_: Bass, _: Bass) -> Bool { return false }
  func hash(into hasher: inout Hasher) {}
}

class Treble: Bass { }

struct Subscripts<T> {
  subscript() -> T {
    get { fatalError() }
    set { fatalError() }
  }
  subscript(generic x: T) -> T {
    get { fatalError() }
    set { fatalError() }
  }
  subscript(concrete x: String) -> String {
    get { fatalError() }
    set { fatalError() }
  }
  subscript(x: String, y: String) -> String {
    get { fatalError() }
    set { fatalError() }
  }
  subscript<U>(subGeneric z: U) -> U {
    get { fatalError() }
    set { fatalError() }
  }
  subscript(mutable x: T) -> T {
    get { fatalError() }
    set { fatalError() }
  }
  subscript(bass: Bass) -> Bass {
    get { return bass }
    set { }
  }
}

struct SubscriptDefaults1 {
  subscript(x: Int = 0) -> Int {
    get { fatalError() }
    set { fatalError() }
  }
  subscript(x: Int, y: Int, z: Int = 0) -> Int {
    get { fatalError() }
    set { fatalError() }
  }
  subscript(x: Bool, bool y: Bool = false) -> Bool {
    get { fatalError() }
    set { fatalError() }
  }
  subscript(bool x: Bool, y: Int, z: Int = 0) -> Int {
    get { fatalError() }
    set { fatalError() }
  }
}

struct SubscriptDefaults2 {
  subscript(x: Int? = nil) -> Int {
    get { fatalError() }
    set { fatalError() }
  }
}

struct SubscriptDefaults3 {
  subscript(x: Int = #line) -> Int {
    get { fatalError() }
    set { fatalError() }
  }
}

struct SubscriptDefaults4 {
  subscript<T : Numeric>(x x: T, y y: T = 0) -> T {
    get { fatalError() }
    set { fatalError() }
  }
}

struct SubscriptDefaults5 {
  subscript<T : ExpressibleByStringLiteral>(x x: T, y y: T =  #function) -> T {
    get { fatalError() }
    set { fatalError() }
  }
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}10subscripts1x1y1syx_q_SStSHRzSHR_r0_lF
func subscripts<T: Hashable, U: Hashable>(x: T, y: U, s: String) {
  _ = \Subscripts<T>.[]
  _ = \Subscripts<T>.[generic: x]
  _ = \Subscripts<T>.[concrete: s]
  _ = \Subscripts<T>.[s, s]
  _ = \Subscripts<T>.[subGeneric: s]
  _ = \Subscripts<T>.[subGeneric: x]
  _ = \Subscripts<T>.[subGeneric: y]

  _ = \Subscripts<U>.[]
  _ = \Subscripts<U>.[generic: y]
  _ = \Subscripts<U>.[concrete: s]
  _ = \Subscripts<U>.[s, s]
  _ = \Subscripts<U>.[subGeneric: s]
  _ = \Subscripts<U>.[subGeneric: x]
  _ = \Subscripts<U>.[subGeneric: y]

  _ = \Subscripts<String>.[]
  _ = \Subscripts<String>.[generic: s]
  _ = \Subscripts<String>.[concrete: s]
  _ = \Subscripts<String>.[s, s]
  _ = \Subscripts<String>.[subGeneric: s]
  _ = \Subscripts<String>.[subGeneric: x]
  _ = \Subscripts<String>.[subGeneric: y]

  _ = \Subscripts<T>.[s, s].count

  _ = \Subscripts<T>.[Bass()]
  _ = \Subscripts<T>.[Treble()]

  _ = \SubscriptDefaults1.[]
  _ = \SubscriptDefaults1.[0]
  _ = \SubscriptDefaults1.[0, 0]
  _ = \SubscriptDefaults1.[0, 0, 0]
  _ = \SubscriptDefaults1.[false]
  _ = \SubscriptDefaults1.[false, bool: false]
  _ = \SubscriptDefaults1.[bool: false, 0]
  _ = \SubscriptDefaults1.[bool: false, 0, 0]
  
  _ = \SubscriptDefaults2.[]
  _ = \SubscriptDefaults2.[0]
  _ = \SubscriptDefaults3.[]
  _ = \SubscriptDefaults3.[0]
  _ = \SubscriptDefaults4.[x: 0]
  _ = \SubscriptDefaults4.[x: 0, y: 0]
  _ = \SubscriptDefaults5.[x: ""]
  _ = \SubscriptDefaults5.[x: "", y: ""]
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}check_default_subscripts
func check_default_subscripts() {
  // CHECK: [[INTX:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 0
  // CHECK: [[IX:%[0-9]+]] = apply %{{[0-9]+}}([[INTX]], {{.*}}
  // CHECK: [[INTY:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 0
  // CHECK: [[IY:%[0-9]+]] = apply %{{[0-9]+}}([[INTY]], {{.*}}
  // CHECK: [[KEYPATH:%[0-9]+]] = keypath $WritableKeyPath<SubscriptDefaults4, Int>, (root $SubscriptDefaults4; settable_property $Int, id @$s8keypaths18SubscriptDefaults4V1x1yxx_xtcSjRzluig : $@convention(method) <τ_0_0 where τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, SubscriptDefaults4) -> @out τ_0_0, getter @$s8keypaths18SubscriptDefaults4V1x1yxx_xtcSjRzluipACSiTK : $@convention(thin) (@in_guaranteed SubscriptDefaults4, UnsafeRawPointer) -> @out Int, setter @$s8keypaths18SubscriptDefaults4V1x1yxx_xtcSjRzluipACSiTk : $@convention(thin) (@in_guaranteed Int, @inout SubscriptDefaults4, UnsafeRawPointer) -> (), indices [%$0 : $Int : $Int, %$1 : $Int : $Int], indices_equals @$sS2iTH : $@convention(thin) (UnsafeRawPointer, UnsafeRawPointer) -> Bool, indices_hash @$sS2iTh : $@convention(thin) (UnsafeRawPointer) -> Int) ([[IX]], [[IY]])
  _ = \SubscriptDefaults4.[x: 0, y: 0]

  // CHECK: [[INTINIT:%[0-9]+]] = integer_literal $Builtin.IntLiteral, 0
  // CHECK: [[I:%[0-9]+]] = apply %{{[0-9]+}}([[INTINIT]], {{.*}}
  // CHECK: [[DFN:%[0-9]+]] = function_ref @$s8keypaths18SubscriptDefaults4V1x1yxx_xtcSjRzluipfA0_ : $@convention(thin) <τ_0_0 where τ_0_0 : Numeric> () -> @out τ_0_0
  // CHECK: [[ALLOC:%[0-9]+]] = alloc_stack $Int
  // CHECK: apply [[DFN]]<Int>([[ALLOC]]) : $@convention(thin) <τ_0_0 where τ_0_0 : Numeric> () -> @out τ_0_0
  // CHECK: [[LOAD:%[0-9]+]] = load [trivial] [[ALLOC]] : $*Int
  // CHECK: [[KEYPATH:%[0-9]+]] = keypath $WritableKeyPath<SubscriptDefaults4, Int>, (root $SubscriptDefaults4; settable_property $Int, id @$s8keypaths18SubscriptDefaults4V1x1yxx_xtcSjRzluig : $@convention(method) <τ_0_0 where τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, SubscriptDefaults4) -> @out τ_0_0, getter @$s8keypaths18SubscriptDefaults4V1x1yxx_xtcSjRzluipACSiTK : $@convention(thin) (@in_guaranteed SubscriptDefaults4, UnsafeRawPointer) -> @out Int, setter @$s8keypaths18SubscriptDefaults4V1x1yxx_xtcSjRzluipACSiTk : $@convention(thin) (@in_guaranteed Int, @inout SubscriptDefaults4, UnsafeRawPointer) -> (), indices [%$0 : $Int : $Int, %$1 : $Int : $Int], indices_equals @$sS2iTH : $@convention(thin) (UnsafeRawPointer, UnsafeRawPointer) -> Bool, indices_hash @$sS2iTh : $@convention(thin) (UnsafeRawPointer) -> Int) ([[I]], [[LOAD]])
  _ = \SubscriptDefaults4.[x: 0]
  
  // CHECK: [[STRX_LIT:%[0-9]+]] = string_literal utf8 ""
  // CHECK: [[STRX:%[0-9]+]] = apply %{{[0-9]+}}([[STRX_LIT]], {{.*}}
  // CHECK: [[STRY_LIT:%[0-9]+]] = string_literal utf8 "check_default_subscripts()"
  // CHECK: [[DEF_ARG:%[0-9]+]] = apply %{{[0-9]+}}([[STRY_LIT]], {{.*}}
  // CHECK: keypath $WritableKeyPath<SubscriptDefaults5, String>, (root $SubscriptDefaults5; settable_property $String, id @$s8keypaths18SubscriptDefaults5V1x1yxx_xtcs26ExpressibleByStringLiteralRzluig : $@convention(method) <τ_0_0 where τ_0_0 : ExpressibleByStringLiteral> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, SubscriptDefaults5) -> @out τ_0_0, getter @$s8keypaths18SubscriptDefaults5V1x1yxx_xtcs26ExpressibleByStringLiteralRzluipACSSTK : $@convention(thin) (@in_guaranteed SubscriptDefaults5, UnsafeRawPointer) -> @out String, setter @$s8keypaths18SubscriptDefaults5V1x1yxx_xtcs26ExpressibleByStringLiteralRzluipACSSTk : $@convention(thin) (@in_guaranteed String, @inout SubscriptDefaults5, UnsafeRawPointer) -> (), indices [%$0 : $String : $String, %$1 : $String : $String], indices_equals @$sS2STH : $@convention(thin) (UnsafeRawPointer, UnsafeRawPointer) -> Bool, indices_hash @$sS2STh : $@convention(thin) (UnsafeRawPointer) -> Int) ([[STRX]], [[DEF_ARG]])
  _ = \SubscriptDefaults5.[x: ""]
  
  // CHECK: [[STRX_LIT:%[0-9]+]] = string_literal utf8 ""
  // CHECK: [[STRX:%[0-9]+]] = apply %{{[0-9]+}}([[STRX_LIT]], {{.*}}
  // CHECK: [[STRY_LIT:%[0-9]+]] = string_literal utf8 ""
  // CHECK: [[STRY:%[0-9]+]] = apply %{{[0-9]+}}([[STRY_LIT]], {{.*}}
  // CHECK: keypath $WritableKeyPath<SubscriptDefaults5, String>, (root $SubscriptDefaults5; settable_property $String, id @$s8keypaths18SubscriptDefaults5V1x1yxx_xtcs26ExpressibleByStringLiteralRzluig : $@convention(method) <τ_0_0 where τ_0_0 : ExpressibleByStringLiteral> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, SubscriptDefaults5) -> @out τ_0_0, getter @$s8keypaths18SubscriptDefaults5V1x1yxx_xtcs26ExpressibleByStringLiteralRzluipACSSTK : $@convention(thin) (@in_guaranteed SubscriptDefaults5, UnsafeRawPointer) -> @out String, setter @$s8keypaths18SubscriptDefaults5V1x1yxx_xtcs26ExpressibleByStringLiteralRzluipACSSTk : $@convention(thin) (@in_guaranteed String, @inout SubscriptDefaults5, UnsafeRawPointer) -> (), indices [%$0 : $String : $String, %$1 : $String : $String], indices_equals @$sS2STH : $@convention(thin) (UnsafeRawPointer, UnsafeRawPointer) -> Bool, indices_hash @$sS2STh : $@convention(thin) (UnsafeRawPointer) -> Int) ([[STRX]], [[STRY]])
  _ = \SubscriptDefaults5.[x: "", y: ""]
}

struct SubscriptVariadic1 {
  subscript(x: Int...) -> Int { x[0] }
}

struct SubscriptVariadic2 {
  subscript<T : ExpressibleByStringLiteral>(x: T...) -> T { x[0] }
}

struct SubscriptVariadic3<T : ExpressibleByStringLiteral> {
  subscript(x: T...) -> T { x[0] }
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}test_variadics
func test_variadics() {
  // CHECK: [[ARR_COUNT:%[0-9]+]] = integer_literal $Builtin.Word, 3
  // CHECK: [[FN_REF:%[0-9]+]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK: [[MAKE_ARR:%[0-9]+]] = apply [[FN_REF]]<Int>([[ARR_COUNT]])
  // CHECK: ([[ARR:%[0-9]+]], %{{[0-9]+}}) = destructure_tuple [[MAKE_ARR]] : $(Array<Int>, Builtin.RawPointer)
  // CHECK: keypath $KeyPath<SubscriptVariadic1, Int>, (root $SubscriptVariadic1; gettable_property $Int,  id @$s8keypaths18SubscriptVariadic1VyS2id_tcig : $@convention(method) (@guaranteed Array<Int>, SubscriptVariadic1) -> Int, getter @$s8keypaths18SubscriptVariadic1VyS2id_tcipACTK : $@convention(thin) (@in_guaranteed SubscriptVariadic1, UnsafeRawPointer) -> @out Int, indices [%$0 : $Array<Int> : $Array<Int>], indices_equals @$sSaySiGTH : $@convention(thin) (UnsafeRawPointer, UnsafeRawPointer) -> Bool, indices_hash @$sSaySiGTh : $@convention(thin) (UnsafeRawPointer) -> Int) ([[ARR]])
  _ = \SubscriptVariadic1.[1, 2, 3]
  // CHECK: [[ARR_COUNT:%[0-9]+]] = integer_literal $Builtin.Word, 1
  // CHECK: [[FN_REF:%[0-9]+]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK: [[MAKE_ARR:%[0-9]+]] = apply [[FN_REF]]<Int>([[ARR_COUNT]])
  // CHECK: ([[ARR:%[0-9]+]], %{{[0-9]+}}) = destructure_tuple [[MAKE_ARR]] : $(Array<Int>, Builtin.RawPointer)
  // CHECK: keypath $KeyPath<SubscriptVariadic1, Int>, (root $SubscriptVariadic1; gettable_property $Int,  id @$s8keypaths18SubscriptVariadic1VyS2id_tcig : $@convention(method) (@guaranteed Array<Int>, SubscriptVariadic1) -> Int, getter @$s8keypaths18SubscriptVariadic1VyS2id_tcipACTK : $@convention(thin) (@in_guaranteed SubscriptVariadic1, UnsafeRawPointer) -> @out Int, indices [%$0 : $Array<Int> : $Array<Int>], indices_equals @$sSaySiGTH : $@convention(thin) (UnsafeRawPointer, UnsafeRawPointer) -> Bool, indices_hash @$sSaySiGTh : $@convention(thin) (UnsafeRawPointer) -> Int) ([[ARR]])
  _ = \SubscriptVariadic1.[1]
  // CHECK: [[ARR_COUNT:%[0-9]+]] = integer_literal $Builtin.Word, 0
  // CHECK: [[FN_REF:%[0-9]+]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK: [[MAKE_ARR:%[0-9]+]] = apply [[FN_REF]]<Int>([[ARR_COUNT]])
  // CHECK: ([[ARR:%[0-9]+]], %{{[0-9]+}}) = destructure_tuple [[MAKE_ARR]] : $(Array<Int>, Builtin.RawPointer)
  // CHECK: keypath $KeyPath<SubscriptVariadic1, Int>, (root $SubscriptVariadic1; gettable_property $Int,  id @$s8keypaths18SubscriptVariadic1VyS2id_tcig : $@convention(method) (@guaranteed Array<Int>, SubscriptVariadic1) -> Int, getter @$s8keypaths18SubscriptVariadic1VyS2id_tcipACTK : $@convention(thin) (@in_guaranteed SubscriptVariadic1, UnsafeRawPointer) -> @out Int, indices [%$0 : $Array<Int> : $Array<Int>], indices_equals @$sSaySiGTH : $@convention(thin) (UnsafeRawPointer, UnsafeRawPointer) -> Bool, indices_hash @$sSaySiGTh : $@convention(thin) (UnsafeRawPointer) -> Int) ([[ARR]])
  _ = \SubscriptVariadic1.[]
  
  _ = \SubscriptVariadic2.["", "1"]
  _ = \SubscriptVariadic2.[""]
  // CHECK: [[ARR_COUNT:%[0-9]+]] = integer_literal $Builtin.Word, 2
  // CHECK: [[FN_REF:%[0-9]+]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK: [[MAKE_ARR:%[0-9]+]] = apply [[FN_REF]]<String>([[ARR_COUNT]])
  // CHECK: ([[ARR:%[0-9]+]], %{{[0-9]+}}) = destructure_tuple [[MAKE_ARR]] : $(Array<String>, Builtin.RawPointer)
  // CHECK: keypath $KeyPath<SubscriptVariadic2, String>, (root $SubscriptVariadic2; gettable_property $String,  id @$s8keypaths18SubscriptVariadic2Vyxxd_tcs26ExpressibleByStringLiteralRzluig : $@convention(method) <τ_0_0 where τ_0_0 : ExpressibleByStringLiteral> (@guaranteed Array<τ_0_0>, SubscriptVariadic2) -> @out τ_0_0, getter @$s8keypaths18SubscriptVariadic2Vyxxd_tcs26ExpressibleByStringLiteralRzluipACSSTK : $@convention(thin) (@in_guaranteed SubscriptVariadic2, UnsafeRawPointer) -> @out String, indices [%$0 : $Array<String> : $Array<String>], indices_equals @$sSaySSGTH : $@convention(thin) (UnsafeRawPointer, UnsafeRawPointer) -> Bool, indices_hash @$sSaySSGTh : $@convention(thin) (UnsafeRawPointer) -> Int) ([[ARR]])
  _ = \SubscriptVariadic2.["", #function]
  
  _ = \SubscriptVariadic3<String>.[""]
  _ = \SubscriptVariadic3<String>.["", "1"]
  _ = \SubscriptVariadic3<String>.[]
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}subclass_generics
func subclass_generics<T: C<Int>, U: C<V>, V/*: PoC*/>(_: T, _: U, _: V) {
  _ = \T.x
  _ = \T.z
  _ = \T.computed
  _ = \T.extension

  _ = \U.x
  _ = \U.z
  _ = \U.computed
  _ = \U.extension

/*
  _ = \V.x
  _ = \V.z
  _ = \V.computed
  _ = \V.extension
 */

  _ = \(C<Int> & P).x
  _ = \(C<Int> & P).z
  _ = \(C<Int> & P).computed
  _ = \(C<Int> & P).extension

  _ = \(C<V> & P).x
  _ = \(C<V> & P).z
  _ = \(C<V> & P).computed
  _ = \(C<V> & P).extension

/* TODO: When we support superclass requirements on protocols, we should test
 * this case as well.
  _ = \PoC.x
  _ = \PoC.z
  _ = \PoC.computed
  _ = \PoC.extension
 */
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}identity
func identity<T>(_: T) {
  // CHECK: keypath $WritableKeyPath<T, T>, <τ_0_0> ({{.*}}root $τ_0_0) <T>
  let _: WritableKeyPath<T, T> = \T.self
  // CHECK: keypath $WritableKeyPath<Array<T>, Array<T>>, <τ_0_0> ({{.*}}root $Array<τ_0_0>) <T>
  let _: WritableKeyPath<[T], [T]> = \[T].self
  // CHECK: keypath $WritableKeyPath<String, String>, ({{.*}}root $String)
  let _: WritableKeyPath<String, String> = \String.self
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}tuples
func tuples(_: T) {
  // CHECK: keypath $WritableKeyPath<T, Int>, (root $T; stored_property #T.a : $(Int, String); tuple_element #0 : $Int)
  let _: WritableKeyPath<T, Int> = \T.a.0
  // CHECK: keypath $WritableKeyPath<T, String>, (root $T; stored_property #T.a : $(Int, String); tuple_element #1 : $String)
  let _: WritableKeyPath<T, String> = \T.a.1
  // CHECK: keypath $KeyPath<T, String>, (root $T; stored_property #T.b : $(f: String, g: Int); tuple_element #0 : $String)
  let _: KeyPath<T, String> = \T.b.f
  // CHECK: keypath $KeyPath<T, Int>, (root $T; stored_property #T.b : $(f: String, g: Int); tuple_element #1 : $Int)
  let _: KeyPath<T, Int> = \T.b.g
  // CHECK: keypath $KeyPath<T, C<Int>>, (root $T; stored_property #T.c : $(x: C<Int>, y: C<String>); tuple_element #0 : $C<Int>)
  let _: KeyPath<T, C<Int>> = \T.c.x
  // CHECK: keypath $KeyPath<T, C<String>>, (root $T; stored_property #T.c : $(x: C<Int>, y: C<String>); tuple_element #1 : $C<String>)
  let _: KeyPath<T, C<String>> = \T.c.y

  // CHECK: keypath $ReferenceWritableKeyPath<T, Int>, (root $T; stored_property #T.c : $(x: C<Int>, y: C<String>); tuple_element #0 : $C<Int>; stored_property #C.x : $Int)
  let _: ReferenceWritableKeyPath<T, Int> = \T.c.x.x
  // CHECK: keypath $KeyPath<T, String>, (root $T; stored_property #T.c : $(x: C<Int>, y: C<String>); tuple_element #0 : $C<Int>; stored_property #C.y : $String)
  let _: KeyPath<T, String> = \T.c.x.y
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}tuples_generic
func tuples_generic<T, U, V>(_: T, _: U, _: V) {
  typealias TUC = (T, U, C<V>)

  // CHECK: keypath $WritableKeyPath<(T, U, C<V>), T>, <τ_0_0, τ_0_1, τ_0_2> (root $(τ_0_0, τ_0_1, C<τ_0_2>); tuple_element #0 : $τ_0_0) <T, U, V>
  let _: WritableKeyPath<TUC, T> = \TUC.0
  // CHECK: keypath $WritableKeyPath<(T, U, C<V>), U>, <τ_0_0, τ_0_1, τ_0_2> (root $(τ_0_0, τ_0_1, C<τ_0_2>); tuple_element #1 : $τ_0_1) <T, U, V>
  let _: WritableKeyPath<TUC, U> = \TUC.1
  // CHECK: keypath $ReferenceWritableKeyPath<(T, U, C<V>), V>, <τ_0_0, τ_0_1, τ_0_2> (root $(τ_0_0, τ_0_1, C<τ_0_2>); tuple_element #2 : $C<τ_0_2>; stored_property #C.x : $τ_0_2) <T, U, V>
  let _: ReferenceWritableKeyPath<TUC, V> = \TUC.2.x
  // CHECK: keypath $KeyPath<(T, U, C<V>), String>, <τ_0_0, τ_0_1, τ_0_2> (root $(τ_0_0, τ_0_1, C<τ_0_2>); tuple_element #2 : $C<τ_0_2>; stored_property #C.y : $String) <T, U, V>
  let _: KeyPath<TUC, String> = \TUC.2.y
}
