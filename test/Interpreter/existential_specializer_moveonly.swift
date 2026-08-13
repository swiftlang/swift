// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift %s -O -o %t/main_opt
// RUN: %target-codesign %t/main_opt
// RUN: %target-run %t/main_opt | %FileCheck %s
// RUN: %target-build-swift %s -Ounchecked -o %t/main_ounchecked
// RUN: %target-codesign %t/main_ounchecked
// RUN: %target-run %t/main_ounchecked | %FileCheck %s

// REQUIRES: executable_test

// Regression test: ExistentialSpecializer clones a function that consumes
// an existential parameter into a version taking the concrete type
// directly, and rewrites the call site by opening the existential and
// copying its payload into a fresh temporary for the specialized callee,
// while separately destroying the original existential container. That's
// fine for a copyable payload (two independent owned copies, each
// destroyed once), but was illegal for a move-only payload: it required
// making a copy of a value that can't be copied, and even if it could,
// the container's separate destroy would double-destroy the payload. Under
// -O this used to either crash the compiler outright or, with the
// unconditional copy patched around, silently double-destroy the payload.

protocol P: ~Copyable {}

final class Canary {
  static var deinitCount = 0
  deinit { Canary.deinitCount += 1 }
}

// Carries a class reference so we can verify the payload is destroyed
// exactly once. Also large enough that the opened-value temporary isn't
// trivially folded away by an earlier pass.
struct Payload: ~Copyable, P {
  let canary: Canary
  var pad0, pad1, pad2, pad3, pad4, pad5: Int
}

@inline(never)
func consumeIt(_ box: consuming any P & ~Copyable) {
}

@inline(never)
func makeAndConsume() {
  consumeIt(Payload(canary: Canary(), pad0: 0, pad1: 0, pad2: 0, pad3: 0,
                    pad4: 0, pad5: 0))
}

makeAndConsume()
// CHECK: deinit count: 1
print("deinit count:", Canary.deinitCount)
