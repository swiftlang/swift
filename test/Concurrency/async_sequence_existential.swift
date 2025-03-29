// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -dump-ast 2>&1 | %FileCheck %s

// REQUIRES: concurrency

extension Error {
  func printMe() { }
}

func test(seq: any AsyncSequence) async {
  // CHECK: "error" interface_type="any Error"
  do {
    for try await _ in seq { }
  } catch {
    error.printMe()
  }
}
