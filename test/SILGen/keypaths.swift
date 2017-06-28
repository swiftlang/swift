// RUN: %target-swift-frontend -enable-experimental-keypath-components -emit-silgen %s | %FileCheck %s

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
  // CHECK-SAME:   getter @_T08keypaths1CC8nonfinalAA1SVyxGvAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in C<τ_0_0>) -> @out S<τ_0_0>,
  // CHECK-SAME:   setter @_T08keypaths1CC8nonfinalAA1SVyxGvAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in S<τ_0_0>, @in C<τ_0_0>) -> ()
  // CHECK-SAME: ) <T>
  _ = \C<T>.nonfinal

  // CHECK: keypath $KeyPath<C<T>, S<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $C<τ_0_0>;
  // CHECK-SAME: gettable_property $S<τ_0_0>,
  // CHECK-SAME:   id #C.computed!getter.1 : <T> (C<T>) -> () -> S<T>,
  // CHECK-SAME:   getter @_T08keypaths1CC8computedAA1SVyxGvAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in C<τ_0_0>) -> @out S<τ_0_0>
  // CHECK-SAME: ) <T>
  _ = \C<T>.computed

  // CHECK: keypath $ReferenceWritableKeyPath<C<T>, S<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $C<τ_0_0>;
  // CHECK-SAME: settable_property $S<τ_0_0>, 
  // CHECK-SAME:   id #C.observed!getter.1 : <T> (C<T>) -> () -> S<T>,
  // CHECK-SAME:   getter @_T08keypaths1CC8observedAA1SVyxGvAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in C<τ_0_0>) -> @out S<τ_0_0>,
  // CHECK-SAME:   setter @_T08keypaths1CC8observedAA1SVyxGvAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in S<τ_0_0>, @in C<τ_0_0>) -> ()
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
  // CHECK-SAME:   getter @_T08keypaths1CC12reabstractedyycvAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in C<τ_0_0>) -> @out @callee_owned (@in ()) -> @out (),
  // CHECK-SAME:   setter @_T08keypaths1CC12reabstractedyycvAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in @callee_owned (@in ()) -> @out (), @in C<τ_0_0>) -> ()
  // CHECK-SAME: ) <T>
  _ = \C<T>.reabstracted

  // CHECK: keypath $KeyPath<S<T>, C<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $S<τ_0_0>; gettable_property $C<τ_0_0>,
  // CHECK-SAME: id @_T08keypaths1SV8computedAA1CCyxGfg : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @owned C<τ_0_0>,
  // CHECK-SAME:   getter @_T08keypaths1SV8computedAA1CCyxGvAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in S<τ_0_0>) -> @out C<τ_0_0>
  // CHECK-SAME: ) <T>
  _ = \S<T>.computed

  // CHECK: keypath $WritableKeyPath<S<T>, C<T>>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $S<τ_0_0>;
  // CHECK-SAME: settable_property $C<τ_0_0>,
  // CHECK-SAME:   id @_T08keypaths1SV8observedAA1CCyxGfg : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @owned C<τ_0_0>,
  // CHECK-SAME:   getter @_T08keypaths1SV8observedAA1CCyxGvAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in S<τ_0_0>) -> @out C<τ_0_0>,
  // CHECK-SAME:   setter @_T08keypaths1SV8observedAA1CCyxGvAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in C<τ_0_0>, @inout S<τ_0_0>) -> ()
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
  // CHECK-SAME:    getter @_T08keypaths1SV12reabstractedyycvAA1PRzlACyxGTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in S<τ_0_0>) -> @out @callee_owned (@in ()) -> @out (),
  // CHECK-SAME:    setter @_T08keypaths1SV12reabstractedyycvAA1PRzlACyxGTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in @callee_owned (@in ()) -> @out (), @inout S<τ_0_0>) -> ()
  // CHECK-SAME: ) <T>
  _ = \S<T>.reabstracted

  // CHECK: keypath $KeyPath<T, Int>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $τ_0_0;
  // CHECK-SAME: gettable_property $Int, 
  // CHECK-SAME:   id #P.x!getter.1 : <Self where Self : P> (Self) -> () -> Int,
  // CHECK-SAME:   getter @_T08keypaths1PP1xSivAaBRzlxTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in τ_0_0) -> @out Int
  // CHECK-SAME: ) <T>
  _ = \T.x
  // CHECK: keypath $WritableKeyPath<T, String>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $τ_0_0;
  // CHECK-SAME: settable_property $String,
  // CHECK-SAME:   id #P.y!getter.1 : <Self where Self : P> (Self) -> () -> String,
  // CHECK-SAME:   getter @_T08keypaths1PP1ySSvAaBRzlxTK : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in τ_0_0) -> @out String,
  // CHECK-SAME:   setter @_T08keypaths1PP1ySSvAaBRzlxTk : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in String, @inout τ_0_0) -> ()
  // CHECK-SAME: ) <T>
  _ = \T.y

  // CHECK: keypath $KeyPath<T, String>, <τ_0_0 where τ_0_0 : P> (
  // CHECK-SAME: root $τ_0_0;
  // CHECK-SAME: gettable_property $String,
  // CHECK-SAME:   id @_T08keypaths1PPAAE1zSSfg
  _ = \T.z
}

struct Concrete: P {
  var x: Int
  var y: String
}

// CHECK-LABEL: sil hidden @_T08keypaths35keyPathsWithSpecificGenericInstanceyyF
func keyPathsWithSpecificGenericInstance() {
  // CHECK: keypath $KeyPath<Concrete, String>, (
  // CHECK-SAME: gettable_property $String,
  // CHECK-SAME:   id @_T08keypaths1PPAAE1zSSfg
  // CHECK-SAME:   getter @_T08keypaths1PPAAE1zSSvAA8ConcreteVTK : $@convention(thin) (@in Concrete) -> @out String
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

// CHECK-LABEL: sil hidden @_T08keypaths18keyPathForOptionalyyF
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
