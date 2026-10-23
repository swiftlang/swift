// RUN: %target-swift-frontend -dump-ast %s -target %target-swift-5.1-abi-triple | %FileCheck %s

// REQUIRES: concurrency

func work() async {}

actor A {
  func asyncDefer() {
    Task {
      // CHECK-LABEL: (func_decl {{.*}} "$defer()" interface_type="() async -> ()" access=private captures=(<isolation>) result="()" thrown_type="<null>"
      defer { await work() }
      _ = self
      await work()
    }
  }

  func syncDeferIsolation() {
    Task {
      var seen: (any Actor)? = nil
      // CHECK-LABEL: (func_decl {{.*}} "$defer()" interface_type="() -> ()" access=private captures=(<isolation> seen<direct><noescape>) result="()" thrown_type="<null>"
      defer { seen = #isolation }
      _ = self
      await work()
      _ = seen
    }
  }

  func plainSyncDefer() {
    Task {
      var x = 0
      // CHECK-LABEL: (func_decl {{.*}} "$defer()" interface_type="() -> ()" access=private captures=(x<direct><noescape>) result="()" thrown_type="<null>"
      defer { x += 1 }
      _ = self
      await work()
    }
  }

  func nestedAsyncDefer() {
    Task {
      _ = self
      // CHECK-LABEL: (func_decl {{.*}} "$defer()" interface_type="() async -> ()" access=private captures=(<isolation>) result="()" thrown_type="<null>"
      defer {
        // CHECK: (func_decl {{.*}} "$defer()" interface_type="() async -> ()" access=private captures=(<isolation>) result="()" thrown_type="<null>"
        defer { await work() }
        await work()
      }
      await work()
    }
  }

  // Outside of closures the actor is captured as a regular capture
  func asyncDeferInMethod() async {
    // CHECK-LABEL: (func_decl {{.*}} "$defer()" interface_type="() async -> ()" access=private captures=(self) result="()" thrown_type="<null>"
    defer { await work() }
    await work()
  }
}
