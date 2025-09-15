// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -strict-concurrency=targeted -target %target-swift-5.1-abi-triple -emit-sil -verify -o /dev/null %s
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -strict-concurrency=complete -verify-additional-prefix complete- -target %target-swift-5.1-abi-triple -emit-sil -verify -o /dev/null %s

// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
struct TL {
  @TaskLocal // expected-note{{in expansion of macro 'TaskLocal' on static property 'number' here}}
  static var number: Int = 0

  @TaskLocal
  static var someNil: Int?

  // expected-note@+1{{in expansion of macro 'TaskLocal' on static property 'noValue' here}}
  @TaskLocal // expected-error{{@TaskLocal' property must have default value, or be optional}}
  static var noValue: Int // expected-note{{'noValue' declared here}}

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
