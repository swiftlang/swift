// RUN: %target-swift-frontend -primary-file %s -g -emit-irgen  -disable-availability-checking | %FileCheck %s
// REQUIRES: concurrency

// CHECK: call i1 (ptr, i1, ...) @llvm.coro.end.async
func foo() async -> Never {
  await bar()
  fatalError()
}

// CHECK: call i1 (ptr, i1, ...) @llvm.coro.end.async
func bar() async -> Never {
  await foo()
  fatalError()
}
