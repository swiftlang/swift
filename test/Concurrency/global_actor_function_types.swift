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

func testConversions(f: @escaping @SomeGlobalActor (Int) -> Void, g: @escaping (Int) -> Void) {
  let _: Int = f // expected-error{{cannot convert value of type '@SomeGlobalActor (Int) -> Void' to specified type 'Int'}}

  let _: (Int) -> Void = f // expected-error{{converting function value of type '@SomeGlobalActor (Int) -> Void' to '(Int) -> Void' loses global actor 'SomeGlobalActor'}}
  let _: @SomeGlobalActor (Int) -> Void = g // okay

  // FIXME: this could be better.
  let _: @OtherGlobalActor (Int) -> Void = f // expected-error{{cannot convert value of type 'SomeGlobalActor' to specified type 'OtherGlobalActor'}}
}
