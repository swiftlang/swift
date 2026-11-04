@MainActor
final class C {
  var n = 0

  func work() async {
    let inheritedMain = {
      self.n += 1
    }
    inheritedMain()

    Task.detached {
      print("detached")
    }
  }
}

// RUN: %sourcekitd-test -req=collect-inferred-isolation %s -- %s | %FileCheck %s

// CHECK:      key.results: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: "closure",
// CHECK-NEXT:     key.offset: {{[0-9]+}},
// CHECK-NEXT:     key.length: {{[0-9]+}},
// CHECK-NEXT:     key.actor_isolation: "@MainActor"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: "closure",
// CHECK-NEXT:     key.offset: {{[0-9]+}},
// CHECK-NEXT:     key.length: {{[0-9]+}},
// CHECK-NEXT:     key.actor_isolation: "@concurrent"
// CHECK-NEXT:   }
// CHECK-NEXT: ]
