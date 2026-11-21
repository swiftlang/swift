// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -strict-concurrency=targeted -target %target-swift-5.1-abi-triple -emit-sil -verify -o /dev/null %s
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -strict-concurrency=complete -verify-additional-prefix complete- -target %target-swift-5.1-abi-triple -emit-sil -verify -o /dev/null %s

// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
struct TL {
  @TaskLocal // expected-note{{in expansion of macro 'TaskLocal' on static property 'number' here}}
  static var number: Int = 0
  /*
  expected-expansion@-2:29{{
    expected-note@1:20{{change 'let' to 'var' to make it mutable}}
  }}
  */

  @TaskLocal
  static var someNil: Int?

  // expected-note@+1{{in expansion of macro 'TaskLocal' on static property 'noValue' here}}
  @TaskLocal // expected-error{{@TaskLocal' property must have default value, or be optional}}
  static var noValue: Int // expected-note{{'noValue' declared here}}
  /*
  expected-expansion@-2:26{{
    expected-error@3:9{{cannot find '$noValue' in scope; did you mean 'noValue'?}}
  }}
  */

  @TaskLocal // expected-error{{'@TaskLocal' can only be applied to 'static' property}}
  var notStatic: String?
}

@TaskLocal
var global: Int = 0

class NotSendable {}

@available(SwiftStdlib 5.1, *)
func test () async {
  TL.number = 10 // expected-error{{cannot assign to property: 'number' is a get-only property}}

  TL.$number = 10 // expected-error{{cannot assign value of type 'Int' to type 'TaskLocal<Int>'}}
  // expected-error@-1{{cannot assign to property: '$number' is a 'let' constant}}

  let _: Int = TL.number
  let _: Int = TL.$number.get()
}

@TaskLocal // expected-error{{'accessor' macro cannot be attached to global function ('test')}}
func test() {}

class X {
  @TaskLocal // expected-error{{'accessor' macro cannot be attached to static method ('test')}}
  static func test() {
  }
}

// withValue doesn't throw errors of its own, so it should preserve the
// operation's thrown error type.
enum TaskLocalTypedError: Error { case boom }

@available(SwiftStdlib 5.1, *)
func test_withValue_typedThrows() async {
  // async: typed error is preserved (`do throws(TaskLocalTypedError)` only
  // type-checks if the error is TaskLocalTypedError, not `any Error`)
  do throws(TaskLocalTypedError) {
    try await TL.$number.withValue(1) { () async throws(TaskLocalTypedError) in
      throw .boom
    }
  } catch {
    let _: TaskLocalTypedError = error
  }

  // sync: same
  do throws(TaskLocalTypedError) {
    try TL.$number.withValue(2) { () throws(TaskLocalTypedError) in
      throw .boom
    }
  } catch {
    let _: TaskLocalTypedError = error
  }

  // non-throwing closure: Failure == Never, so no `try` needed
  let _: Int = await TL.$number.withValue(3) { () async in TL.number }
  let _: Int = TL.$number.withValue(4) { TL.number }
}

@available(SwiftStdlib 5.1, *)
func test_withValue_untypedStillWorks() async throws {
  // Failure == any Error: existing code keeps compiling
  try await TL.$number.withValue(5) { () async throws in throw TaskLocalTypedError.boom }
  try TL.$number.withValue(6) { () throws in throw TaskLocalTypedError.boom }
}