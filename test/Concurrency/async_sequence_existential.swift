// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -primary-file %s -dump-ast -o %t.ast.txt
// RUN: %FileCheck %s < %t.ast.txt

// REQUIRES: concurrency

extension Error {
  func printMe() { }
}

func test(seq: any AsyncSequence) async {
  // CHECK-LABEL: (catch_stmts
  // CHECK:         (var_decl {{.*}} "error" interface_type="any Error"
  do {
    for try await _ in seq { }
  } catch {
    error.printMe()
  }
}
