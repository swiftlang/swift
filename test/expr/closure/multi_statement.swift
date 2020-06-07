// RUN: %target-typecheck-verify-swift -swift-version 5 -experimental-multi-statement-closures

func isInt<T>(_ value: T) -> Bool {
  return value is Int
}

func mapWithMoreStatements(ints: [Int]) {
  let _ = ints.map { i in
    let value = i + 1
    do {
      if isInt(i) {
        print(value)
      } else if value == 17 {
        print("seventeen!")
      }
    }
    return String(value)
  }
}
