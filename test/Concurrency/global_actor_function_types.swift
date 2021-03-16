// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

actor SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

@globalActor
struct OtherGlobalActor {
  static let shared = SomeActor()
}

func testConversions(f: @SomeGlobalActor (Int) -> Void) {
  let _: Int = f // expected-error{{cannot convert value of type '@SomeGlobalActor (Int) -> Void' to specified type 'Int'}}
}
