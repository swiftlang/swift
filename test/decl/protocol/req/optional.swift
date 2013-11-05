// RUN: %swift -parse %s -verify

protocol P1 {
  @optional def method(x: Int)

  @optional var prop: Int

  @optional subscript (i: Int) -> Int
}

// @optional cannot be used on non-protocol declarations
@optional var optError: Int = 10 // expected-error{{'optional' attribute can only be applied to protocol members}}

@optional struct optErrorStruct { // expected-error{{'optional' attribute can only be applied to protocol members}}
  @optional var ivar: Int // expected-error{{'optional' attribute can only be applied to protocol members}}
  @optional def foo() { } // expected-error{{'optional' attribute can only be applied to protocol members}}
}

@optional class optErrorClass { // expected-error{{'optional' attribute can only be applied to protocol members}}
  @optional var ivar: Int // expected-error{{'optional' attribute can only be applied to protocol members}}
  @optional def foo() { } // expected-error{{'optional' attribute can only be applied to protocol members}}
}
  
protocol optErrorProtocol {
  @optional typealias Assoc // expected-error{{invalid attributes specified for typealias}}
}
