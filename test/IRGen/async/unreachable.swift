// RUN: %target-swift-frontend -primary-file %s -g -emit-ir -enable-experimental-concurrency -disable-llvm-optzns -disable-swift-specific-llvm-optzns | %FileCheck %s
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
