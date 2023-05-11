// RUN: %target-swift-frontend %use_no_opaque_pointers -primary-file %s -g -emit-ir  -disable-availability-checking -disable-llvm-optzns -disable-swift-specific-llvm-optzns | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -g -emit-ir  -disable-availability-checking -disable-llvm-optzns -disable-swift-specific-llvm-optzns
// REQUIRES: concurrency

// CHECK: call i1 (i8*, i1, ...) @llvm.coro.end.async
func foo() async -> Never {
  await bar()
  fatalError()
}

// CHECK: call i1 (i8*, i1, ...) @llvm.coro.end.async
func bar() async -> Never {
  await foo()
  fatalError()
}
