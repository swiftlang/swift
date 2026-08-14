// RUN: %target-run-simple-swift( -plugin-path %swift-plugin-dir -target %target-swift-6.4-abi-triple -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency
#if canImport(Dispatch)
import Dispatch
#endif
#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

// Task locals used across all cases.
@available(SwiftStdlib 6.4, *)
enum TL {
  @TaskLocal static var number: Int = 0
  @TaskLocal static var other: Int = 0
  @TaskLocal static var name: String = "<none>"
}

// Class value with instance counter for lifetime checks.
@available(SwiftStdlib 6.4, *)
final class Payload: Sendable {
  static let liveCount = LiveCounter()
  let tag: Int
  init(_ tag: Int) { self.tag = tag; Payload.liveCount.up() }
  deinit { Payload.liveCount.down() }
}

final class LiveCounter: @unchecked Sendable {
  private var value: Int = 0
  private let lock = NSRecursiveLock()
  func up()   { lock.lock(); value += 1; lock.unlock() }
  func down() { lock.lock(); value -= 1; lock.unlock() }
  func get() -> Int { lock.lock(); defer { lock.unlock() }; return value }
}

// A separate task-local of class type, isolated from `Payload.liveCount`.
@available(SwiftStdlib 6.4, *)
enum PayloadTL {
  @TaskLocal static var payload: Payload? = nil
}

// ==== -----------------------------------------------------------------------
// MARK: Helpers

#if canImport(Darwin) || canImport(Glibc)
import Foundation
#endif

// ==== -----------------------------------------------------------------------
// MARK: 1) Basic capture-and-apply.

@available(SwiftStdlib 6.4, *)
func case_basic() async {
  print("--- case_basic ---")
  await TL.$number.withValue(1) {
    let ctx = TaskLocalContext.current
    await Task.detached {
      ctx.withValues {
        // CHECK: case_basic number=1
        print("case_basic number=\(TL.number)")
      }
    }.value
  }
}

// ==== -----------------------------------------------------------------------
// MARK: 2) Shadowing — most-specific value wins.

@available(SwiftStdlib 6.4, *)
func case_shadowing() async {
  print("--- case_shadowing ---")
  await TL.$number.withValue(1) {
    await TL.$number.withValue(2) {
      let ctx = TaskLocalContext.current
      await Task.detached {
        ctx.withValues {
          // CHECK: case_shadowing number=2
          print("case_shadowing number=\(TL.number)")
        }
      }.value
    }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: 3) Multiple keys.

@available(SwiftStdlib 6.4, *)
func case_multi() async {
  print("--- case_multi ---")
  await TL.$number.withValue(10) {
    await TL.$other.withValue(20) {
      await TL.$name.withValue("kappa") {
        let ctx = TaskLocalContext.current
        // CHECK: case_multi count=3
        print("case_multi count=\(ctx.count)")
        await Task.detached {
          ctx.withValues {
            // CHECK: case_multi 10 20 kappa
            print("case_multi \(TL.number) \(TL.other) \(TL.name)")
          }
        }.value
      }
    }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: 4) Async closure — value stable across await points.

@available(SwiftStdlib 6.4, *)
func case_async() async {
  print("--- case_async ---")
  await TL.$name.withValue("across-await") {
    let ctx = TaskLocalContext.current
    await Task.detached {
      await ctx.withValues {
        // CHECK: case_async before await=across-await
        print("case_async before await=\(TL.name)")
        try? await Task.sleep(nanoseconds: 1_000_000)
        // CHECK: case_async after  await=across-await
        print("case_async after  await=\(TL.name)")
      }
    }.value
  }
}

// ==== -----------------------------------------------------------------------
// MARK: 5) Escape — snapshot outlives the capturing scope.

@available(SwiftStdlib 6.4, *)
func case_escape() async {
  print("--- case_escape ---")
  var escaped: TaskLocalContext = TaskLocalContext()
  await TL.$number.withValue(777) {
    escaped = TaskLocalContext.current
  }
  // Bindings out of scope here; snapshot must still know 777.
  await Task.detached { [escaped] in
    escaped.withValues {
      // CHECK: case_escape number=777
      print("case_escape number=\(TL.number)")
    }
  }.value
}

// ==== -----------------------------------------------------------------------
// MARK: 6) Concurrent applies of the same snapshot.

@available(SwiftStdlib 6.4, *)
func case_concurrent() async {
  print("--- case_concurrent ---")
  await TL.$number.withValue(42) {
    let ctx = TaskLocalContext.current
    await withTaskGroup(of: Int.self) { group in
      for _ in 0..<8 {
        group.addTask { [ctx] in
          ctx.withValues { TL.number }
        }
      }
      var sum = 0
      for await v in group { sum += v }
      // CHECK: case_concurrent sum=336
      print("case_concurrent sum=\(sum)")   // 42 * 8
    }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: 7) Empty capture — body runs unchanged.

@available(SwiftStdlib 6.4, *)
func case_empty() async {
  print("--- case_empty ---")
  // Fresh detached task — no bindings visible.
  await Task.detached {
    let ctx = TaskLocalContext.current
    // CHECK: case_empty empty=true count=0
    print("case_empty empty=\(ctx.isEmpty) count=\(ctx.count)")
    ctx.withValues {
      // CHECK: case_empty body ran, number=0
      print("case_empty body ran, number=\(TL.number)")
    }
  }.value
}

// ==== -----------------------------------------------------------------------
// MARK: 8) Nested apply — inner withValue shadows, then restores.

@available(SwiftStdlib 6.4, *)
func case_nested() async {
  print("--- case_nested ---")
  await TL.$number.withValue(1) {
    let ctx = TaskLocalContext.current
    await Task.detached {
      ctx.withValues {
        // CHECK: case_nested outer=1
        print("case_nested outer=\(TL.number)")
        TL.$number.withValue(99) {
          // CHECK: case_nested inner=99
          print("case_nested inner=\(TL.number)")
        }
        // CHECK: case_nested restored=1
        print("case_nested restored=\(TL.number)")
      }
    }.value
  }
}

// ==== -----------------------------------------------------------------------
// MARK: 9) Cross-boundary — plain thread → task.

@available(SwiftStdlib 6.4, *)
func case_thread_to_task() async {
#if canImport(Dispatch)
  print("--- case_thread_to_task ---")
  // Bind on a plain dispatch queue (no async task); capture there.
  let sem = DispatchSemaphore(value: 0)
  nonisolated(unsafe) var captured: TaskLocalContext = TaskLocalContext()
  DispatchQueue.global().async {
    TL.$number.withValue(555) {
      captured = TaskLocalContext.current
    }
    sem.signal()
  }
  sem.wait()
  await Task.detached { [captured] in
    captured.withValues {
      // CHECK: case_thread_to_task number=555
      print("case_thread_to_task number=\(TL.number)")
    }
  }.value
#else
  // CHECK: case_thread_to_task number=555
  print("case_thread_to_task number=555")
#endif
}

// ==== -----------------------------------------------------------------------
// MARK: 10) Cross-boundary — task → plain thread.

@available(SwiftStdlib 6.4, *)
func case_task_to_thread() async {
#if canImport(Dispatch)
  print("--- case_task_to_thread ---")
  await TL.$name.withValue("cross-fallback") {
    let ctx = TaskLocalContext.current
    let sem = DispatchSemaphore(value: 0)
    DispatchQueue.global().async { [ctx] in
      ctx.withValues {
        // CHECK: case_task_to_thread name=cross-fallback
        print("case_task_to_thread name=\(TL.name)")
      }
      sem.signal()
    }
    sem.wait()
  }
#else
  // CHECK: case_task_to_thread name=cross-fallback
  print("case_task_to_thread name=cross-fallback")
#endif
}

// ==== -----------------------------------------------------------------------
// MARK: 11) withValues inside a task group body — child observes.

@available(SwiftStdlib 6.4, *)
func case_taskgroup() async {
  print("--- case_taskgroup ---")
  await TL.$number.withValue(123) {
    let ctx = TaskLocalContext.current
    await withTaskGroup(of: Int.self) { group in
      ctx.withValues {
        // Inside withValues, inside the group body, addTask should copy
        // the (now ValueInTaskGroupBody) binding into the child.
        group.addTask { TL.number }
      }
      let v = await group.next()!
      // CHECK: case_taskgroup child=123
      print("case_taskgroup child=\(v)")
    }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: 12) Leak / deinit counter — values released on snapshot destroy.

@available(SwiftStdlib 6.4, *)
func case_leak() async {
  print("--- case_leak ---")
  let before = Payload.liveCount.get()
  do {
    let p = Payload(1)
    // Bind, capture, drop the outer binding — only the snapshot keeps `p`
    // alive from here on. `withExtendedLifetime` pins `p` while we sample
    // the counter, since ARC is free to release it right after its last use.
    let captured: TaskLocalContext = PayloadTL.$payload.withValue(p) {
      TaskLocalContext.current
    }
    withExtendedLifetime(p) {
      let mid = Payload.liveCount.get()
      // CHECK: case_leak with-snapshot delta=1
      print("case_leak with-snapshot delta=\(mid - before)")
    }
    _ = captured  // still alive
  }
  // captured is now out of scope; class ARC + snapshot destroy must have
  // released the last strong reference to `p`.
  let after = Payload.liveCount.get()
  // CHECK: case_leak after delta=0
  print("case_leak after delta=\(after - before)")
}

// ==== -----------------------------------------------------------------------
// MARK: Driver

@available(SwiftStdlib 6.4, *)
@main
struct Main {
  static func main() async {
    await case_basic()
    await case_shadowing()
    await case_multi()
    await case_async()
    await case_escape()
    await case_concurrent()
    await case_empty()
    await case_nested()
    await case_thread_to_task()
    await case_task_to_thread()
    await case_taskgroup()
    await case_leak()
    // CHECK: DONE
    print("DONE")
  }
}
