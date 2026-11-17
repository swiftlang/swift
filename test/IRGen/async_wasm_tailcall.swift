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

// Runtime entry declarations and calls must agree on the async context.
// RUN: %target-swift-frontend %s -emit-ir -module-name test -disable-availability-checking -Xcc -mtail-call | %FileCheck %s --check-prefix=RUNTIME-ABI
// RUN: %target-swift-frontend %s -emit-ir -module-name test -disable-availability-checking | %FileCheck %s --check-prefix=RUNTIME-ABI
// RUN: %target-swift-frontend %s -S -module-name test -disable-availability-checking -Xcc -mtail-call | %FileCheck %s --check-prefix=RUNTIME-ABI-ASM

public actor RuntimeABIActor {
  public func update() {}
}

public func hopToActor(_ actor: RuntimeABIActor) async {
  await actor.update()
}

public func awaitChild() async -> Int {
  async let child = callee()
  return await child
}

// RUNTIME-ABI-DAG: declare {{(swiftcc|swifttailcc)}} void @swift_task_switch(ptr swiftasync, ptr, i32, i32)
// RUNTIME-ABI-DAG: declare {{(swiftcc|swifttailcc)}} void @swift_asyncLet_finish(ptr swiftasync, ptr, ptr, ptr, ptr)
// RUNTIME-ABI-DAG: call {{(swiftcc|swifttailcc)}} void @swift_task_switch(ptr {{[^,]*}}swiftasync
// RUNTIME-ABI-DAG: call {{(swiftcc|swifttailcc)}} void @swift_asyncLet_finish(ptr {{[^,]*}}swiftasync
// RUNTIME-ABI-ASM-DAG: .functype swift_task_switch (i32, i32, i32, i32, i32, i32) -> ()
// RUNTIME-ABI-ASM-DAG: .functype swift_asyncLet_finish (i32, i32, i32, i32, i32, i32, i32) -> ()
