// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -default-isolation MainActor -emit-sil -swift-version 5 -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -default-isolation MainActor -emit-sil -swift-version 5 -strict-concurrency=complete -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -default-isolation MainActor -emit-sil -swift-version 6 -parse-as-library %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_DefaultIsolationPerFile

using nonisolated

// CHECK: nonisolated struct S
// CHECK: nonisolated class C
// CHECK: nonisolated enum E
// CHECK: nonisolated extension S
// CHECK: nonisolated struct Outer
// CHECK-NOT: @MainActor struct Inner
// CHECK: nonisolated struct MixedIsolation
// CHECK: @MainActor class IsoBase
// CHECK: nonisolated class Sub
// CHECK: @MainActor class IsoTarget
// CHECK: nonisolated extension IsoTarget
// CHECK: nonisolated let globalLet
// CHECK: nonisolated struct Stored

struct S {
  // CHECK: // S.method()
  // CHECK-NEXT: // Isolation: nonisolated
  func method() {}
}

class C {
  // CHECK: // C.method()
  // CHECK-NEXT: // Isolation: nonisolated
  func method() {}
}

enum E {
  case a
  // CHECK: // E.method()
  // CHECK-NEXT: // Isolation: nonisolated
  func method() {}
}

extension S {
  // CHECK: // S.extMethod()
  // CHECK-NEXT: // Isolation: nonisolated
  func extMethod() {}
}

// CHECK: // freeFunc()
// CHECK-NEXT: // Isolation: nonisolated
func freeFunc() {}

// CHECK: // computedGlobal.getter
// CHECK-NEXT: // Isolation: nonisolated
var computedGlobal: Int { 0 }

struct Outer {
  struct Inner {
    // CHECK: // Outer.Inner.nestedMethod()
    // CHECK-NEXT: // Isolation: unspecified
    func nestedMethod() {}
  }
}

// CHECK: // explicitMain()
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
@MainActor func explicitMain() {}

// CHECK: // concurrentFunc()
// CHECK-NEXT: // Isolation: @concurrent
@concurrent func concurrentFunc() async {}

struct MixedIsolation {
  // CHECK: // MixedIsolation.mainMethod()
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  @MainActor func mainMethod() {}

  // CHECK: // MixedIsolation.defaultMethod()
  // CHECK-NEXT: // Isolation: nonisolated
  func defaultMethod() {}
}

@MainActor class IsoBase {}

final class Sub: IsoBase {
  // CHECK: // Sub.subMethod()
  // CHECK-NEXT: // Isolation: nonisolated
  func subMethod() {}
}

@MainActor class IsoTarget {}

extension IsoTarget {
  // CHECK: // IsoTarget.extMethodOnIso()
  // CHECK-NEXT: // Isolation: nonisolated
  func extMethodOnIso() {}
}

// CHECK: // globalLet.unsafeMutableAddressor
// CHECK-NEXT: // Isolation: unspecified
let globalLet = 5

struct Stored {
  // CHECK: // Stored.storedLet.getter
  // CHECK-NEXT: // Isolation: unspecified
  let storedLet = 1

  // CHECK: // Stored.storedVar.getter
  // CHECK-NEXT: // Isolation: unspecified
  var storedVar = 2
}
