// RUN: %target-typecheck-buildpartialblock-verify-swift

@resultBuilder
struct ValidBuilder1 {
  static func buildPartialBlock(first: Int) -> Int { fatalError() }
  static func buildPartialBlock(accumulated: Int, next: Int) -> Int { fatalError() }
}

@resultBuilder
struct ValidBuilder2 {
  @available(*, unavailable)
  static func buildPartialBlock(first: Int) -> Int { fatalError() }
  @available(*, unavailable)
  static func buildPartialBlock(accumulated: Int, next: Int) -> Int { fatalError() }
  // available
  // static func buildBlock(_: Int...) -> Int { fatalError() }
}

func caller1_ValidBuilder2() {
  // expected-error @+1 {{result builder 'ValidBuilder2' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @ValidBuilder2 var x: Int {
    1
    1
    1
  }
}

@resultBuilder
struct InvalidBuilder1 {} // expected-error {{result builder must provide at least one static 'buildBlock' method, or both 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)'}}

@resultBuilder
struct InvalidBuilder2 { // expected-error {{result builder must provide at least one static 'buildBlock' method, or both 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)'}}
  static func buildPartialBlock(first: Any) -> Any { fatalError() }
}
@resultBuilder
struct InvalidBuilder3 { // expected-error {{result builder must provide at least one static 'buildBlock' method, or both 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)'}}
  static func buildPartialBlock(accumulated: Any, next: Any) -> Any { fatalError() }
}
