// RUN: %target-swift-emit-silgen %s -verify -swift-version 6

struct S: Equatable {
  static func foo() -> Self { fatalError() }
  static func bar(_ x: Int) -> Self { fatalError() }
  static func baz(x: Int, y: Int) -> Self { fatalError() }
  public static func == (_: Self, _: Self) -> Bool { false }
}

// rdar://139234188 - Make sure we don't consider these members to be partially
// applied for concurrency adjustment.
func foo(_ x: S) {
  _ = {
    switch x {
    case .foo():
      break
    case .bar(0):
      break
    case .baz(x: 1, y: 2):
      break
    default:
      break
    }
  }
}
