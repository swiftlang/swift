// RUN: mkdir -p %t
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -disable-availability-checking %s -dump-ast > %t/ast.log 2>&1
// RUN: cat %t/ast.log

// RUN: %target-swift-frontend  -disable-availability-checking %s -dump-ast 2>&1 | %FileCheck %s

// REQUIRES: concurrency

// Fails with an unoptimized stdlib currently.
// rdar://128858036
//
// REQUIRES: optimized_stdlib

extension Error {
  func printMe() { }
}

// CHECK: "test(seq:)"
func test(seq: any AsyncSequence) async {
  // CHECK: "error" interface type="any Error"
  do {
    for try await _ in seq { }
  } catch {
    error.printMe()
  }
}
