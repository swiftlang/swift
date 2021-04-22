// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
enum TL {
  @TaskLocal(default: 0)
  static var number // expected-note{{setter for 'number' has been explicitly marked unavailable here}}

  // TODO: ban non-static task-local definitions
//  @TaskLocal(default: 0)
//  @available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
//  var notStatic
}

@TaskLocal(default: 0) // expected-error{{property wrappers are not yet supported in top-level code}}
var global // expected-error{{type annotation missing in pattern}}

class NotSendable {}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
enum T2 {
  @TaskLocal()
  static var notSendable: TaskLocal<NotSendable?>.Access // expected-error{{'NotSendable' does not conform to protocol 'Sendable'}}
  // expected-note@-1{{requirement from conditional conformance of 'NotSendable?' to 'Sendable'}}
  // expected-note@-2{{requirement specified as 'NotSendable' : 'Sendable'}}
  // expected-error@-3{{'TaskLocal' requires that 'NotSendable' conform to 'Sendable'}}

  @TaskLocal(default: NotSendable()) // expected-error{{generic class 'TaskLocal' requires that 'NotSendable' conform to 'Sendable'}}
  static var notSendable2
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test () async {
  TL.number = 10 // expected-error{{cannot assign value of type 'Int' to type 'TaskLocal<Int>.Access'}}
  TL.number = TL.number // expected-error{{setter for 'number' is unavailable}}
  let int: Int = TL.number // expected-error{{cannot convert value of type 'TaskLocal<Int>.Access' to specified type 'Int'}}
  let ok: Int = TL.number.get()
}
