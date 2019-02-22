// RUN: not %target-swift-frontend -typecheck %s -swift-version 4
// RUN: not %target-swift-frontend -typecheck %s -swift-version 5

public struct Horse : OptionSet {
  typealias Element = Horse // this was not public...
  public static let horse = Horse()
}
