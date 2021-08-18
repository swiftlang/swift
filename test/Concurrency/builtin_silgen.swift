// RUN: %target-swift-frontend  -disable-availability-checking %s -parse-as-library -parse-stdlib -emit-sil -o - | %FileCheck %s

// REQUIRES: concurrency

import _Concurrency

@MainActor
func suspend() async {}

// Builtin.hopToActor should generate a mandator hop_to_executor
// before releaseing the actor and reaching a suspend.
//
// CHECK-LABEL: sil private @$s14builtin_silgen11runDetachedyyFyyYaYbcfU_ : $@convention(thin) @Sendable @async () -> () {
// CHECK:   [[ACTOR:%.*]] = apply %1(%0) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK:   hop_to_executor [mandatory] [[ACTOR]] : $MainActor
// CHECK:   strong_release [[ACTOR]] : $MainActor
// CHECK:   apply %{{.*}}() : $@convention(thin) @async () -> ()
// CHECK-LABEL: } // end sil function '$sIeghH_ytIeghHr_TR'
@available(SwiftStdlib 5.5, *)
func runDetached() {
  Task.detached {
    Builtin.hopToActor(MainActor.shared)
    await suspend()
  }
}
