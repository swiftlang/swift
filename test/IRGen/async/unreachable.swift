// RUN: %target-swift-frontend -primary-file %s -g -emit-irgen  -target %target-swift-5.1-abi-triple | %FileCheck %s
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
