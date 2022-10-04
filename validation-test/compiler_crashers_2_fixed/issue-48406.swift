// RUN: not %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/48406

extension Dictionary {
  func doSomething<T>() -> [T : Value] {
    let pairs: [(T, Value)] = []
    return Dictionary(uniqueKeysWithValues: pairs)
  }
}
