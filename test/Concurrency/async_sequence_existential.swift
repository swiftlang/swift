// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify

// RUN: %target-swift-frontend  -disable-availability-checking %s -dump-ast 2>&1 | %FileCheck %s

// REQUIRES: concurrency

extension Error {
  func printMe() { }
}

func test(seq: any AsyncSequence) async {
  // CHECK: "error" interface type="any Error"
  do {
    for try await _ in seq { }
  } catch {
    error.printMe()
  }
}
