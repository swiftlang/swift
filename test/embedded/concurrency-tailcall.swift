// Verify that embedded async coroutine splitting on Wasm uses
// musttail/return_call with the tail-call feature and regular calls without it.

// IR-level checks:
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test %s -emit-ir | %FileCheck %s --check-prefix=NOTAIL
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test %s -emit-ir -Xcc -mtail-call | %FileCheck %s --check-prefix=TAIL

// Assembly-level checks:
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test %s -S -Xcc -mtail-call | %FileCheck %s --check-prefix=TAIL-ASM
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test %s -S | %FileCheck %s --check-prefix=NOTAIL-ASM

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

func callee() async -> Int { return 42 }

public func caller() async -> Int {
  return await callee()
}

// Without -mtail-call: async functions use swiftcc, no musttail
// NOTAIL: define {{.*}}swiftcc void @"$e4test6callerSiyYaF"(ptr swiftasync
// NOTAIL-NOT: musttail call

// With -mtail-call: async functions use swifttailcc with musttail
// TAIL: define {{.*}}swifttailcc void @"$e4test6callerSiyYaF"(ptr swiftasync
// TAIL: musttail call swifttailcc void

// Assembly with -mtail-call: return_call Wasm instruction
// TAIL-ASM: return_call

// Assembly without -mtail-call: no return_call Wasm instruction
// NOTAIL-ASM: .functype
// NOTAIL-ASM-NOT: return_call
