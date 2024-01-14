// RUN: %target-swift-frontend -strict-concurrency=targeted -disable-availability-checking -emit-sil -verify -o /dev/null %s
// RUN: %target-swift-frontend -strict-concurrency=complete -verify-additional-prefix complete- -disable-availability-checking -emit-sil -verify -o /dev/null %s
// RUN: %target-swift-frontend -strict-concurrency=complete -verify-additional-prefix complete- -disable-availability-checking -emit-sil -verify -o /dev/null %s -enable-experimental-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

@available(SwiftStdlib 5.1, *)
struct TL {
  @TaskLocal
  static var number: Int = 0 // expected-complete-warning {{static property 'number' is not concurrency-safe because it is non-isolated global shared mutable state; this is an error in Swift 6}}
  // expected-complete-note@-1 {{isolate 'number' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}

  @TaskLocal
  static var someNil: Int? // expected-complete-warning {{static property 'someNil' is not concurrency-safe because it is non-isolated global shared mutable state; this is an error in Swift 6}}
  // expected-complete-note@-1 {{isolate 'someNil' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}

  @TaskLocal
  static var noValue: Int // expected-error{{'static var' declaration requires an initializer expression or an explicitly stated getter}}
  // expected-note@-1{{add an initializer to silence this error}}
  // expected-complete-warning@-2 {{static property 'noValue' is not concurrency-safe because it is non-isolated global shared mutable state; this is an error in Swift 6}}
  // expected-complete-note@-3 {{isolate 'noValue' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}

  @TaskLocal
  var notStatic: String? // expected-error{{property 'notStatic', must be static because property wrapper 'TaskLocal<String?>' can only be applied to static properties}}
}

@TaskLocal // expected-error{{property wrappers are not yet supported in top-level code}}
var global: Int = 0

class NotSendable {}

@available(SwiftStdlib 5.1, *)
func test () async {
  TL.number = 10 // expected-error{{cannot assign to property: 'number' is a get-only property}}
  TL.$number = 10 // expected-error{{cannot assign value of type 'Int' to type 'TaskLocal<Int>'}}
  let _: Int = TL.number
  let _: Int = TL.$number.get()
}
