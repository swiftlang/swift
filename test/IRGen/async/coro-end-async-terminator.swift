// RUN: %target-swift-frontend -primary-file %s -emit-irgen | %FileCheck %s
// REQUIRES: concurrency

// The normal-return path of an async function must end with `ret void`
// after `coro.end.async`, not `unreachable`. Mid-pipeline function passes
// treat `unreachable` as "nothing after this point is observable", which
// lets DSE delete stores to the caller-owned indirect result buffer and
// miscompiles async functions whose result is stored from an earlier
// block (e.g. a task-group child returning a tuple; swiftlang/swift#92698).

// CHECK: call void (ptr, i1, ...) @llvm.coro.end.async
// CHECK-NEXT: ret void
public func returnsInt() async -> Int {
  await Task.yield()
  return 42
}

// CHECK: call void (ptr, i1, ...) @llvm.coro.end.async
// CHECK-NEXT: ret void
public func returnsTuple() async -> (Int, Int, Int, Int) {
  await Task.yield()
  return (1, 2, 3, 4)
}
