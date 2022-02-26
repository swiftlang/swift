// RUN: %target-typecheck-buildpartialblock-verify-swift

// REQUIRES: macosx

@resultBuilder
struct ValidBuilder2 {
  @available(SwiftStdlib 5.6, *)
  static func buildPartialBlock(first: Int) -> Int { fatalError() }
  @available(SwiftStdlib 5.6, *)
  static func buildPartialBlock(accumulated: Int, next: Int) -> Int { fatalError() }
}

@available(SwiftStdlib 5.5, *)
func caller1_ValidBuilder2() {
  // expected-error @+1 {{result builder 'ValidBuilder2' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @ValidBuilder2 var x: Int {
    1
    1
    1
  }
  if #available(SwiftStdlib 5.6, *) {
    @ValidBuilder2 var y: Int {
      1
      1
      1
    }
  }
}
