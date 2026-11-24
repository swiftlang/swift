// RUN: %target-swift-frontend -emit-sil -Onone -g %s -o /dev/null

// Make sure the MovedAsyncVarDebugInfoPropagator reaches a fixpoint when
// different undef representatives for the same variable circulate around a
// loop. This used to hang the compiler.

// REQUIRES: concurrency

func g(_ x: inout String) async throws {}

func f(_ a: String?) async {
  if var x = a {
    do {
      try await g(&x)
      _ = consume x
    } catch {}
  }
  while Bool.random() {
    if Bool.random() {
      print("")
    }
  }
}
