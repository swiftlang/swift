// RUN: %target-typecheck-verify-swift

@_functionBuilder // expected-warning{{'@_functionBuilder' has been renamed to '@resultBuilder'}}{{2-18=resultBuilder}}
struct MyBuilder {
  static func buildBlock(_: Any...) -> Any { }
}
