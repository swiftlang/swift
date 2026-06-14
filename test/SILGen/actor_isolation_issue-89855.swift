// RUN: %target-swift-emit-silgen -Xllvm -sil-print-function-isolation-info %s -swift-version 6 -target %target-swift-5.1-abi-triple | %FileCheck %s

// REQUIRES: concurrency

// Regression test for https://github.com/swiftlang/swift/issues/89855

// CHECK-LABEL: sil private [isolation "@concurrent"] [ossa] @$s4main1CC1fyyScA_pYiYaFyycfU_yyYacfU_ : $@convention(thin) @async (@guaranteed C) -> () {
final class C {
  func f(_ other: isolated any Actor) async {
    let _ = {
      _ = other
      let _ = { [self] () async in
        // Previously, this closure would be "self-isolated" to a non-actor value.
        _ = self
      }
    }
  }
}

// CHECK-LABEL: sil private [isolation "actor_instance"] [ossa] @$s4main1AC1fyyScA_pYiFyycfU_yycfU_ : $@convention(thin) (@guaranteed A, @sil_isolated @guaranteed any Actor) -> () {
actor A {
    func f(_ other: isolated any Actor) {
        let otherIso = {
            _ = other
            let bug = { [self] in
              // Previously, this closure would be "self-isolated" instead of "other-isolated".
              _ = self
              _ = other
            }
            bug()
        }
        otherIso()
    }
}
