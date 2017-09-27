// RUN: not %target-typecheck-verify-swift

extension Dictionary {
  func doSomething<T>() -> [T : Value] {
    let pairs: [(T, Value)] = []
    return Dictionary(uniqueKeysWithValues: pairs)
  }
}
