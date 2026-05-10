// RUN: %target-swift-frontend -emit-silgen %s  -target %target-swift-5.1-abi-triple | %FileCheck %s
// REQUIRES: concurrency

class BaseClass<T> {
  func wait() async -> T {}
  func waitOrDie() async throws -> T {}
  // TODO: test sync override of async method (https://github.com/swiftlang/swift/issues/88367)
  // func retry() async {}
  var isReady: Bool { get async {} }
  subscript(waitingOn i: Int) -> T { get async {} }
}

class Derived : BaseClass<Int> {
  override func wait() async -> Int {}
  override func waitOrDie() async -> Int {}
  // override func retry() {}
  override var isReady: Bool { get {} }
  override subscript(waitingOn i: Int) -> Int { get {} }
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s18async_vtable_thunk7DerivedC4waitSiyYaFAA9BaseClassCADxyYaFTV : $@convention(method) @async (@guaranteed Derived) -> @out Int {

// CHECK-LABEL: sil private [thunk] [ossa] @$s18async_vtable_thunk7DerivedC7isReadySbvgAA9BaseClassCADSbvgTV : $@convention(method) @async (@guaranteed Derived) -> Bool {

// CHECK-LABEL: sil private [thunk] [ossa] @$s18async_vtable_thunk7DerivedC9waitingOnS2i_tcigAA9BaseClassCADxSi_tcigTV : $@convention(method) @async (Int, @guaranteed Derived) -> @out Int {

// CHECK-LABEL: sil_vtable Derived {
// CHECK:  #BaseClass.wait: <T> (BaseClass<T>) -> () async -> T : @$s18async_vtable_thunk7DerivedC4waitSiyYaFAA9BaseClassCADxyYaFTV [override]
// CHECK-NEXT:  #BaseClass.waitOrDie: <T> (BaseClass<T>) -> () async throws -> T : @$s18async_vtable_thunk7DerivedC9waitOrDieSiyYaFAA9BaseClassCADxyYaKFTV [override]
// CHECK-NEXT:  #BaseClass.isReady!getter: <T> (BaseClass<T>) -> () async -> Bool : @$s18async_vtable_thunk7DerivedC7isReadySbvgAA9BaseClassCADSbvgTV [override]
// CHECK-NEXT:  #BaseClass.subscript!getter: <T> (BaseClass<T>) -> (Int) async -> T : @$s18async_vtable_thunk7DerivedC9waitingOnS2i_tcigAA9BaseClassCADxSi_tcigTV [override]
// CHECK-NEXT:  #BaseClass.init!allocator: <T> (BaseClass<T>.Type) -> () -> BaseClass<T> : @$s18async_vtable_thunk7DerivedCACycfC [override]
// CHECK-NEXT:  #Derived.waitOrDie: (Derived) -> () async -> Int : @$s18async_vtable_thunk7DerivedC9waitOrDieSiyYaF
// CHECK-NEXT:  #Derived.isReady!getter: (Derived) -> () -> Bool : @$s18async_vtable_thunk7DerivedC7isReadySbvg
// CHECK-NEXT:  #Derived.subscript!getter: (Derived) -> (Int) -> Int : @$s18async_vtable_thunk7DerivedC9waitingOnS2i_tcig
// CHECK-NEXT:  #Derived.deinit!deallocator: @$s18async_vtable_thunk7DerivedCfD
// CHECK-NEXT: }

