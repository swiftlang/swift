// Verify that async coroutine splitting on Wasm uses musttail/return_call
// with the tail-call feature and regular calls without it.

// IR-level checks:
// RUN: %target-swift-frontend %s -emit-ir -module-name test -disable-availability-checking | %FileCheck %s --check-prefix=NOTAIL
// RUN: %target-swift-frontend %s -emit-ir -module-name test -disable-availability-checking -Xcc -mtail-call | %FileCheck %s --check-prefix=TAIL

// Assembly-level checks:
// RUN: %target-swift-frontend %s -S -module-name test -disable-availability-checking -Xcc -mtail-call | %FileCheck %s --check-prefix=TAIL-ASM
// RUN: %target-swift-frontend %s -S -module-name test -disable-availability-checking | %FileCheck %s --check-prefix=NOTAIL-ASM

// REQUIRES: concurrency
// REQUIRES: CPU=wasm32

func callee() async -> Int { return 42 }

public func caller() async -> Int {
  return await callee()
}

// Without -mtail-call: async functions use swiftcc, no musttail
// NOTAIL: define {{.*}}swiftcc void @"$s4test6callerSiyYaF"(ptr swiftasync
// NOTAIL-NOT: musttail call

// With -mtail-call: async functions use swifttailcc with musttail
// TAIL: define {{.*}}swifttailcc void @"$s4test6callerSiyYaF"(ptr swiftasync
// TAIL: musttail call swifttailcc void

// Assembly with -mtail-call: return_call Wasm instruction
// TAIL-ASM: return_call

// Assembly without -mtail-call: no return_call Wasm instruction
// NOTAIL-ASM: .functype
// NOTAIL-ASM-NOT: return_call
