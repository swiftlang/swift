
// RUN: %target-swift-frontend -module-name keypaths -emit-silgen -enable-sil-ownership %s | %FileCheck %s

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

/* TODO: When we support superclass requirements on protocols, we should test
 * this case as well.
protocol PoC : C<Int> {}
*/

// CHECK-LABEL: sil hidden @{{.*}}storedProperties
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

// CHECK-LABEL: sil hidden @{{.*}}computedProperties
func computedProperties<T: P>(_: T) {
  // CHECK: keypath $ReferenceWritableKeyPath<C<T>, S<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $C<τ_0_0>;
  // CHECK-SAME: settable_property $S<τ_0_0>, 
  // CHECK-SAME:   id #C.nonfinal!getter.1 : <T> (C<T>) -> () -> S<T>,
  // CHECK-SAME:   getter @$S8keypaths1CC8nonfinalAA1SVyxGvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed C<τ_0_0>) -> @out S<τ_0_0>,
  // CHECK-SAME:   setter @$S8keypaths1CC8nonfinalAA1SVyxGvpAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed S<τ_0_0>, @in_guaranteed C<τ_0_0>) -> ()
  // CHECK-SAME: ) <T>
  _ = \C<T>.nonfinal

  // CHECK: keypath $KeyPath<C<T>, S<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $C<τ_0_0>;
  // CHECK-SAME: gettable_property $S<τ_0_0>,
  // CHECK-SAME:   id #C.computed!getter.1 : <T> (C<T>) -> () -> S<T>,
  // CHECK-SAME:   getter @$S8keypaths1CC8computedAA1SVyxGvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed C<τ_0_0>) -> @out S<τ_0_0>
  // CHECK-SAME: ) <T>
  _ = \C<T>.computed

  // CHECK: keypath $ReferenceWritableKeyPath<C<T>, S<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $C<τ_0_0>;
  // CHECK-SAME: settable_property $S<τ_0_0>, 
  // CHECK-SAME:   id #C.observed!getter.1 : <T> (C<T>) -> () -> S<T>,
  // CHECK-SAME:   getter @$S8keypaths1CC8observedAA1SVyxGvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed C<τ_0_0>) -> @out S<τ_0_0>,
  // CHECK-SAME:   setter @$S8keypaths1CC8observedAA1SVyxGvpAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed S<τ_0_0>, @in_guaranteed C<τ_0_0>) -> ()
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
  // CHECK-SAME:   getter @$S8keypaths1CC12reabstractedyycvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed C<τ_0_0>) -> @out @callee_guaranteed (@in_guaranteed ()) -> @out (),
  // CHECK-SAME:   setter @$S8keypaths1CC12reabstractedyycvpAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed @callee_guaranteed (@in_guaranteed ()) -> @out (), @in_guaranteed C<τ_0_0>) -> ()
  // CHECK-SAME: ) <T>
  _ = \C<T>.reabstracted

  // CHECK: keypath $KeyPath<S<T>, C<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $S<τ_0_0>; gettable_property $C<τ_0_0>,
  // CHECK-SAME: id @$S8keypaths1SV8computedAA1CCyxGvg : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @owned C<τ_0_0>,
  // CHECK-SAME:   getter @$S8keypaths1SV8computedAA1CCyxGvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed S<τ_0_0>) -> @out C<τ_0_0>
  // CHECK-SAME: ) <T>
  _ = \S<T>.computed

  // CHECK: keypath $WritableKeyPath<S<T>, C<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $S<τ_0_0>;
  // CHECK-SAME: settable_property $C<τ_0_0>,
  // CHECK-SAME:   id @$S8keypaths1SV8observedAA1CCyxGvg : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @owned C<τ_0_0>,
  // CHECK-SAME:   getter @$S8keypaths1SV8observedAA1CCyxGvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed S<τ_0_0>) -> @out C<τ_0_0>,
  // CHECK-SAME:   setter @$S8keypaths1SV8observedAA1CCyxGvpAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed C<τ_0_0>, @inout S<τ_0_0>) -> ()
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
  // CHECK-SAME:    getter @$S8keypaths1SV12reabstractedyycvpAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed S<τ_0_0>) -> @out @callee_guaranteed (@in_guaranteed ()) -> @out (),
  // CHECK-SAME:    setter @$S8keypaths1SV12reabstractedyycvpAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed @callee_guaranteed (@in_guaranteed ()) -> @out (), @inout S<τ_0_0>) -> ()
  // CHECK-SAME: ) <T>
  _ = \S<T>.reabstracted

  // CHECK: keypath $KeyPath<T, Int>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $τ_0_0;
  // CHECK-SAME: gettable_property $Int, 
  // CHECK-SAME:   id #P.x!getter.1 : <Self where Self : P> (Self) -> () -> Int,
  // CHECK-SAME:   getter @$S8keypaths1PP1xSivpAaBRzlxTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out Int
  // CHECK-SAME: ) <T>
  _ = \T.x
  // CHECK: keypath $WritableKeyPath<T, String>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $τ_0_0;
  // CHECK-SAME: settable_property $String,
  // CHECK-SAME:   id #P.y!getter.1 : <Self where Self : P> (Self) -> () -> String,
  // CHECK-SAME:   getter @$S8keypaths1PP1ySSvpAaBRzlxTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out String,
  // CHECK-SAME:   setter @$S8keypaths1PP1ySSvpAaBRzlxTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed String, @inout τ_0_0) -> ()
  // CHECK-SAME: ) <T>
  _ = \T.y

  // CHECK: keypath $KeyPath<T, String>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $τ_0_0;
  // CHECK-SAME: gettable_property $String,
  // CHECK-SAME:   id @$S8keypaths1PPAAE1zSSvg
  _ = \T.z
}

struct Concrete: P {
  var x: Int
  var y: String
}

// CHECK-LABEL: sil hidden @$S8keypaths35keyPathsWithSpecificGenericInstanceyyF
func keyPathsWithSpecificGenericInstance() {
  // CHECK: keypath $KeyPath<Concrete, String>, (
  // CHECK-SAME: gettable_property $String,
  // CHECK-SAME:   id @$S8keypaths1PPAAE1zSSvg
  // CHECK-SAME:   getter @$S8keypaths1PPAAE1zSSvpAA8ConcreteVTK : $@convention(thin) (@in_guaranteed Concrete) -> @out String
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

// CHECK-LABEL: sil hidden @$S8keypaths18keyPathForOptionalyyF
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

// CHECK-LABEL: sil hidden @{{.*}}keyPathForStorageQualified
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

// CHECK-LABEL: sil hidden @{{.*}}11iuoKeyPaths
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
  var hashValue: Int { return 0 }
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

// CHECK-LABEL: sil hidden @{{.*}}10subscripts
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
}

// CHECK-LABEL: sil hidden @{{.*}}subclass_generics
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
