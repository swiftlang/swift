// RUN: %target-typecheck-verify-swift  -disable-availability-checking
// REQUIRES: concurrency

@available(SwiftStdlib 5.5, *)
struct TL {
  @TaskLocal
  static var number: Int = 0

  @TaskLocal
  static var someNil: Int?

  @TaskLocal
  static var noValue: Int // expected-error{{'static var' declaration requires an initializer expression or an explicitly stated getter}}
  // expected-note@-1{{add an initializer to silence this error}}

  @TaskLocal
  var notStatic: String? // expected-error{{property 'notStatic', must be static because property wrapper 'TaskLocal<String?>' can only be applied to static properties}}
}

@TaskLocal // expected-error{{property wrappers are not yet supported in top-level code}}
var global: Int = 0

class NotSendable {}

@available(SwiftStdlib 5.5, *)
func test () async {
  TL.number = 10 // expected-error{{cannot assign to property: 'number' is a get-only property}}
  TL.$number = 10 // expected-error{{cannot assign value of type 'Int' to type 'TaskLocal<Int>'}}
  let _: Int = TL.number
  let _: Int = TL.$number.get()
}
