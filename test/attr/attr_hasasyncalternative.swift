// REQUIRES: concurrency

// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -experimental-has-async-alternative-attribute

@hasAsyncAlternative
func func1() {}

@hasAsyncAlternative("betterFunc")
func func2() {}

@hasAsyncAlternative("betterFunc(param:)")
func func3() {}

@hasAsyncAlternative("not+identifier") // expected-error {{argument of 'hasAsyncAlternative' attribute must be an identifier or full function name}}
func func4() {}

@hasAsyncAlternative("$dollarname") // expected-error {{argument of 'hasAsyncAlternative' attribute must be an identifier or full function name}}
func func5() {}

@hasAsyncAlternative("TypePrefix.func") // expected-error {{argument of 'hasAsyncAlternative' attribute must be an identifier or full function name}}
func func6() {}

@hasAsyncAlternative("interpreted \()") // expected-error {{argument of 'hasAsyncAlternative' cannot be an interpolated string literal}}
func func7() {}

@hasAsyncAlternative("missingRParen" // expected-error {{expected ')' in 'hasAsyncAlternative' attribute}}
func func8() {}

@hasAsyncAlternative // expected-note {{attribute already specified here}}
@hasAsyncAlternative("other") // expected-error {{duplicate attribute}}
func duplicate() {}

@hasAsyncAlternative // expected-error {{'@hasAsyncAlternative' attribute cannot be applied to this declaration}}
protocol SomeProto {
  @hasAsyncAlternative
  func protoFunc()
}

@hasAsyncAlternative // expected-error {{'@hasAsyncAlternative' attribute cannot be applied to this declaration}}
struct SomeStruct: SomeProto {
  func protoFunc() { }

  @hasAsyncAlternative
  func structFunc() { }

  @hasAsyncAlternative
  static func staticStructFunc() { }
}

@hasAsyncAlternative // expected-error {{'@hasAsyncAlternative' attribute cannot be applied to this declaration}}
class SomeClass: SomeProto {
  func protoFunc() { }

  @hasAsyncAlternative
  func classFunc() { }
}
