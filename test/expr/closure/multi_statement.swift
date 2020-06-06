// RUN: %target-typecheck-verify-swift -swift-version 5 -experimental-multi-statement-closures

func mapWithMoreStatements(ints: [Int]) {
  let _ = ints.map { i in
    let value = i + 1
    print(value)
    return String(value)
  }
}
