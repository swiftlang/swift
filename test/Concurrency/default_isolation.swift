// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: concurrency
// REQUIRES: swift_feature_DefaultIsolationPerFile

// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 6 -disable-availability-checking %t/main.swift %t/concurrent.swift | %FileCheck %s

//--- main.swift

using @MainActor

class C {
  // CHECK: // static C.shared.getter
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  static let shared = C()

  // CHECK: // C.init()
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  init() {}
}

// CHECK: // test()
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
func test() {
  // CHECK: // closure #1 in test()
  // CHECK-NEXT: // Isolation: nonisolated
  Task.detached {
    let s = S(value: 0)
  }
}

// Tested below. This used to fail in default-isolation mode because
// the type-checker applied the default isolation to the implicit $defer
// function, causing it to have MainActor isolation despite the enclosing
// context being nonisolated.
nonisolated func test_defer() {
  defer {}
}

//--- concurrent.swift

using nonisolated

// CHECK: // S.init(value:)
// CHECK-NEXT: // Isolation: unspecified
struct S {
  var value: Int
}

// CHECK: // test_defer()
// CHECK-NEXT: // Isolation: nonisolated

// CHECK: // $defer #1 () in test_defer()
// CHECK-NEXT: // Isolation: nonisolated

// CHECK: // S.value.getter
// CHECK-NEXT: // Isolation: unspecified
