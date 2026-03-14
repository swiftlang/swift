// RUN: %target-typecheck-verify-swift

@_functionBuilder // expected-warning{{'@_functionBuilder' has been renamed to '@resultBuilder'}}{{2-18=resultBuilder}}
struct MyBuilder {
  static func buildBlock(_: Any...) -> Any { }
}

// rdar://104384604 - empty result builder in swiftinterface file
@resultBuilder
public struct TestInvalidBuildBlock1 {
  // expected-error@-1 {{result builder must provide at least one static 'buildBlock' as accessible as result builder type 'TestInvalidBuildBlock1' (which is public)}}
  static func buildBlock(_: Int) -> Int { 42 }
}

@resultBuilder
public struct TestInvalidBuildBlock2 { // Ok
  static func buildBlock(_: Int) -> Int { 42 }
  public static func buildBlock(_: String) -> String { "" }
}

@resultBuilder
public struct TestInvalidBuildPartialBlockFirst1 {
  // expected-error@-1 {{result builder must provide at least one static 'buildPartialBlock(first:)' as accessible as result builder type 'TestInvalidBuildPartialBlockFirst1' (which is public)}}
  static func buildPartialBlock(first: Int) -> Int { first }
  public static func buildPartialBlock(accumulated: Int, next: Int) -> Int { accumulated + next }
}

@resultBuilder
public struct TestInvalidBuildPartialBlockFirst2 { // Ok
  static func buildPartialBlock(first: Int) -> Int { first }
  public static func buildPartialBlock<T>(first: T) -> T { first }
  public static func buildPartialBlock(accumulated: Int, next: Int) -> Int { accumulated + next }
}

@resultBuilder
public struct TestInvalidBuildPartialBlockAccumulated1 {
  // expected-error@-1 {{result builder must provide at least one static 'buildPartialBlock(accumulated:next:)' as accessible as result builder type 'TestInvalidBuildPartialBlockAccumulated1' (which is public)}}
  public static func buildPartialBlock(first: Int) -> Int { first }
  private static func buildPartialBlock(accumulated: Int, next: Int) -> Int { accumulated + next }
}

@resultBuilder
public struct TestInvalidBuildPartialBlockAccumulated2 { // Ok
  public static func buildPartialBlock<T>(first: T) -> T { first }
  public static func buildPartialBlock<T>(accumulated: T, next: T) -> T { fatalError() }

  private static func buildPartialBlock(accumulated: Int, next: Int) -> Int { accumulated + next }
}

@resultBuilder
public struct TestBuildPartialBlock1 { // Ok
  public static func buildPartialBlock(first: Int) -> Int { first }
  public static func buildPartialBlock(accumulated: Int, next: Int) -> Int { accumulated + next }
}

@resultBuilder
public struct TestBuildPartialBlock2 { // Ok
  private static func buildBlock(_: Int) -> Int { 42 }

  public static func buildPartialBlock(first: Int) -> Int { first }
  public static func buildPartialBlock(accumulated: Int, next: Int) -> Int { accumulated + next }
}

@resultBuilder
public struct TestBuildPartialBlock3 { // Ok
  public static func buildBlock(_: Int) -> Int { 42 }

  fileprivate static func buildPartialBlock(first: Int) -> Int { first }
  fileprivate static func buildPartialBlock(accumulated: Int, next: Int) -> Int { accumulated + next }
}

@resultBuilder
public struct TestBuildPartialBlock4 { // Ok
  public static func buildBlock(_: Int) -> Int { 42 }

  public static func buildPartialBlock(first: Int) -> Int { first }
  public static func buildPartialBlock(accumulated: Int, next: Int) -> Int { accumulated + next }
}
