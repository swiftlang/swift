// RUN: %target-swift-frontend -emit-silgen -verify %s | %FileCheck %s

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
  var reabstracted: () -> ()

  init() { fatalError() }
}

// CHECK-LABEL: sil hidden @{{.*}}storedProperties
func storedProperties<T>(_: T) {
  // CHECK: keypath $KeyPath<S<T>, T> (stored_property #S.x : $T)
  _ = #keyPath2(S<T>, .x)
  // CHECK: keypath $KeyPath<S<T>, String> (stored_property #S.y : $String)
  _ = #keyPath2(S<T>, .y)
  // CHECK: keypath $KeyPath<S<T>, T> (stored_property #S.z : $C<T>, stored_property #C.x : $T)
  _ = #keyPath2(S<T>, .z.x)
  // CHECK: keypath $KeyPath<C<T>, T> (stored_property #C.x : $T)
  _ = #keyPath2(C<T>, .x)
  // CHECK: keypath $KeyPath<C<T>, String> (stored_property #C.y : $String)
  _ = #keyPath2(C<T>, .y)
  // CHECK: keypath $KeyPath<C<T>, T> (stored_property #C.z : $S<T>, stored_property #S.x : $T)
  _ = #keyPath2(C<T>, .z.x)
  // CHECK: keypath $KeyPath<C<T>, String> (stored_property #C.z : $S<T>, stored_property #S.z : $C<T>, stored_property #C.y : $String)
  _ = #keyPath2(C<T>, .z.z.y)
}

func computedProperties<T>(_: T) {
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(C<T>, .nonfinal)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(C<T>, .computed)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(C<T>, .observed)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(C<T>, .nonfinal.x)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(C<T>, .computed.x)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(C<T>, .observed.x)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(C<T>, .z.computed)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(C<T>, .z.observed)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(C<T>, .observed.x)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(C<T>, .reabstracted)

  // expected-error@+1{{not implemented}}
  _ = #keyPath2(S<T>, .computed)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(S<T>, .observed)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(S<T>, .z.nonfinal)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(S<T>, .z.computed)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(S<T>, .z.observed)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(S<T>, .computed.x)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(S<T>, .computed.y)
  // expected-error@+1{{not implemented}}
  _ = #keyPath2(S<T>, .reabstracted)
}
