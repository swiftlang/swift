// RUN: %target-swift-frontend -emit-silgen %s  -target %target-swift-5.1-abi-triple | %FileCheck %s
// REQUIRES: concurrency

class BaseClass<T> {
  func wait() async -> T {}
  func waitOrDie() async throws -> T {}
}

class Derived : BaseClass<Int> {
  override func wait() async -> Int {}
  override func waitOrDie() async -> Int {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s18async_vtable_thunk7DerivedC4waitSiyYaFAA9BaseClassCADxyYaFTV : $@convention(method) @async (@guaranteed Derived) -> @out Int {

// CHECK-LABEL: sil_vtable Derived {
// CHECK:  #BaseClass.wait: <T> (BaseClass<T>) -> () async -> T : @$s18async_vtable_thunk7DerivedC4waitSiyYaFAA9BaseClassCADxyYaFTV [override]
// CHECK-NEXT:  #BaseClass.waitOrDie: <T> (BaseClass<T>) -> () async throws -> T : @$s18async_vtable_thunk7DerivedC9waitOrDieSiyYaFAA9BaseClassCADxyYaKFTV [override]
// CHECK-NEXT:  #BaseClass.init!allocator: <T> (BaseClass<T>.Type) -> () -> BaseClass<T> : @$s18async_vtable_thunk7DerivedCACycfC [override]
// CHECK-NEXT:  #Derived.waitOrDie: (Derived) -> () async -> Int : @$s18async_vtable_thunk7DerivedC9waitOrDieSiyYaF
// CHECK-NEXT:  #Derived.deinit!deallocator: @$s18async_vtable_thunk7DerivedCfD
// CHECK-NEXT: }

