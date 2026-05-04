// RUN: %target-swift-emit-silgen %s

// Exercises a SILGen code path that would trigger an assertion.

public struct Anchor<Value> {}

extension Anchor where Value == String {
  @backDeployed(before: macOS 26.0)
  public static func fixedGenericParam() -> Value {
      return "hello"
  }

  @backDeployed(before: macOS 26.0)
  public static func generic<T>(_ t: T) -> T {
      return t
  }
}
