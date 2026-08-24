// Emitting the metadata of a non-copyable type also emits its value witnesses,
// and the destroy witness calls the deinits of the type and its members. In
// Embedded Swift those deinits must be fully specialized, because unspecialized
// generic functions take type metadata, which doesn't exist there.
//
// `@export(interface)` is what makes a type's metadata emitted eagerly, so the
// types below use it rather than the module-wide `CodeGenerationModel=interface`
// (which is not a `Features.def` feature and so can't be spelled in a REQUIRES).

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

// A concrete instantiation of a generic type is never among the types SILGen
// records, so it has to be discovered from the SIL. `LocalOnly<Int>` is only
// ever a local and `GenericBox<Int>`'s container is itself generic, so neither
// is reachable from a recorded non-generic type — yet both get a specialized
// deinit.
// SIL-DAG: sil_moveonlydeinit $LocalOnly<Int> {
// SIL-DAG: sil_moveonlydeinit $GenericBox<Int> {
public struct LocalOnly<T>: ~Copyable {
  var p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1) }
  deinit { p.deallocate() }
}

public struct GenericBox<T>: ~Copyable {
  var q: UnsafeMutablePointer<Int>
  init() { q = .allocate(capacity: 1) }
  deinit { q.deallocate() }
}

public func useLocalOnly() {
  let s = LocalOnly<Int>()
  _ = s.p
  let g = GenericBox<Int>()
  _ = g.q
}
