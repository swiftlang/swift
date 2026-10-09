@MainActor
final class C {
  var n = 0

  func work() async {
    let inheritedMain = {
      self.n += 1
    }
    _ = inheritedMain

    Task.detached {
      print("detached")
    }
  }
}

// Range covering only the first closure.
// We should see only the @MainActor entry.
//
// RUN: %sourcekitd-test -req=collect-inferred-isolation -pos=6:1 -length=60 %s -- %s | %FileCheck %s --check-prefix=INHERITED

// INHERITED:      key.results: [
// INHERITED-NEXT:   {
// INHERITED-NEXT:     key.kind: "closure",
// INHERITED-NEXT:     key.offset: {{[0-9]+}},
// INHERITED-NEXT:     key.length: {{[0-9]+}},
// INHERITED-NEXT:     key.actor_isolation: "@MainActor"
// INHERITED-NEXT:   }
// INHERITED-NEXT: ]

// Range covering only the second closure.
// see only the nonisolated entry; the first closure must be filtered out.
//
// RUN: %sourcekitd-test -req=collect-inferred-isolation -pos=9:1 -length=100 %s -- %s | %FileCheck %s --check-prefix=DETACHED

// DETACHED:      key.results: [
// DETACHED-NEXT:   {
// DETACHED-NEXT:     key.kind: "closure",
// DETACHED-NEXT:     key.offset: {{[0-9]+}},
// DETACHED-NEXT:     key.length: {{[0-9]+}},
// DETACHED-NEXT:     key.actor_isolation: "@concurrent"
// DETACHED-NEXT:   }
// DETACHED-NEXT: ]
