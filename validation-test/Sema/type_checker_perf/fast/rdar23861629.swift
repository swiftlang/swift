// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: tools-release,no_asan

struct S { var s: String? }

func rdar23861629(_ a: [S]) {
  _ = a.reduce("") {
    ($0 == "") ? ($1.s ?? "") : ($0 + "," + ($1.s ?? "")) + ($1.s ?? "test") + ($1.s ?? "okay")
  }
}
