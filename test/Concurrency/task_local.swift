// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
enum TL {
  @TaskLocal
  static var number: Int = 0

  @TaskLocal
  static var someNil: Int?

  // TODO: ban non-static task-local definitions
//  @TaskLocal(default: 0)
//  @available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
//  var notStatic
}

@TaskLocal // expected-error{{property wrappers are not yet supported in top-level code}}
var global: Int = 0

class NotSendable {}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
enum T2 {
  @TaskLocal // expected-error{{generic class 'TaskLocal' requires that 'NotSendable' conform to 'Sendable'}}
  static var notSendable: NotSendable?

  @TaskLocal // expected-error{{generic class 'TaskLocal' requires that 'NotSendable' conform to 'Sendable'}}
  static var notSendable2: NotSendable = NotSendable()
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test () async {
  TL.number = 10 // expected-error{{cannot assign to property: 'number' is a get-only property}}
  TL.$number = 10 // expected-error{{cannot assign value of type 'Int' to type 'TaskLocal<Int>'}}
  let _: Int = TL.number
  let _: Int = TL.$number.get()
}
