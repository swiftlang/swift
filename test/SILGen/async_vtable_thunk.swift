// RUN: %target-swift-frontend -emit-silgen %s  -disable-availability-checking | %FileCheck %s
// REQUIRES: concurrency

class BaseClass<T> {
  func wait() async -> T {}
}

class Derived : BaseClass<Int> {
  override func wait() async -> Int {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s18async_vtable_thunk7DerivedC4waitSiyYaFAA9BaseClassCADxyYaFTV : $@convention(method) @async (@guaranteed Derived) -> @out Int {

