// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -emit-sil -o - | %FileCheck -check-prefix SIL %s
// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -emit-ir -o - | %FileCheck -check-prefix IR %s

// REQUIRES: swift_feature_Embedded

public struct Storage<T>: ~Copyable {
  var p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1) }
  deinit { p.deallocate() }
}

// A specialized deinit is created for the concrete Storage<Int> ...
// SIL-DAG: sil{{.*}} @$e4main7StorageVfDSi_Tg5 : $@convention(method) (@owned Storage<Int>) -> ()

// ... and registered as the deinit to use for that type.
// SIL-DAG: sil_moveonlydeinit $Storage<Int> {
// SIL-DAG:   @$e4main7StorageVfDSi_Tg5

// The destroy value witness of a container calls the specialized deinit
// directly, with no metadata argument.
// IR-LABEL: define {{.*}} @"$e4main3BoxVwxx"
// IR: call{{.*}} @"$e4main7StorageVfDSi_Tg5"(ptr
// IR: ret void
@export(interface)
public struct Box: ~Copyable {
  var items: Storage<Int>
  init() { items = Storage<Int>() }
}

// Enum payloads are destroyed by the enum's value witnesses too.
// IR-LABEL: define {{.*}} @"$e4main9MaybeUsedOwxx"
// IR: call{{.*}} @"$e4main7StorageVfDSi_Tg5"(ptr
@export(interface)
public enum MaybeUsed: ~Copyable {
  case none
  case some(Storage<Int>)
}

// A type is not required to be referenced anywhere for its metadata to be
// emitted, so an unused container must work as well.
// IR-LABEL: define {{.*}} @"$e4main6UnusedVwxx"
// IR: call{{.*}} @"$e4main7StorageVfDSi_Tg5"(ptr
@export(interface)
public struct Unused: ~Copyable {
  var items: Storage<Int>
}

// Forming an existential is the other thing that makes IRGen emit metadata for
// a type, so the deinit of the concrete type has to be specialized there too.
// A concrete instantiation of a generic type is never among the types SILGen
// recorded, so this one is found from the `init_existential` instruction.
// SIL-DAG: sil_moveonlydeinit $Existentialized<Int> {
public protocol HasF: ~Copyable {
  func f()
}

public struct Existentialized<T>: ~Copyable, HasF {
  var q: UnsafeMutablePointer<Int>
  init() { q = .allocate(capacity: 1) }
  public func f() {}
  deinit { q.deallocate() }
}

@inline(never)
func take(_ e: consuming any HasF & ~Copyable) { e.f() }

public func useExistential() {
  take(Existentialized<Int>())
}

// A type that is merely a local, with no metadata emitted for it, needs no
// specialized deinit: nothing will ever call it through a value witness.
// SIL-NOT: sil_moveonlydeinit $LocalOnly<Int> {
public struct LocalOnly<T>: ~Copyable {
  var p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1) }
  deinit { p.deallocate() }
}

public func useLocalOnly() {
  let s = LocalOnly<Int>()
  _ = s.p
}
