// RUN: not --crash %target-typecheck-verify-swift
// REQUIRES: asserts

extension Dictionary {
  func doSomething<T>() -> [T : Value] {
    let pairs: [(T, Value)] = []
    return Dictionary(uniqueKeysWithValues: pairs)
  }
}
