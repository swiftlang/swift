// RUN: %target-typecheck-verify-swift -swift-version 5

let _: ImplicitlyUnwrappedOptional<Int> = 1 // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{8-36=}}{{39-39=!}}{{39-40=}}
let _: ImplicitlyUnwrappedOptional = 1 // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' in unsupported; use an explicit type followed by '!'}}

extension ImplicitlyUnwrappedOptional {} // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}

func function(
  _: ImplicitlyUnwrappedOptional<Int> // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{6-34=}}{{37-37=!}}{{37-38=}}
) -> ImplicitlyUnwrappedOptional<Int> { // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{6-34=}}{{37-37=!}}{{37-38=}}
  return 1
}

func genericFunction<T>(
  iuo: ImplicitlyUnwrappedOptional<T> // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{8-36=}}{{37-37=!}}{{37-38=}}
) -> ImplicitlyUnwrappedOptional<T> { // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{6-34=}}{{35-35=!}}{{35-36=}}
  return iuo
}

protocol P {
  associatedtype T
  associatedtype U
}

struct S : P {
  typealias T = ImplicitlyUnwrappedOptional<Int> // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  typealias U = Optional<ImplicitlyUnwrappedOptional<Int>> // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}

  subscript (
    index: ImplicitlyUnwrappedOptional<Int> // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{12-40=}}{{43-43=!}}{{43-44=}}
  )     -> ImplicitlyUnwrappedOptional<Int> { // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{12-40=}}{{43-43=!}}{{43-44=}}
    return index
  }
}

func generic<T : P>(_: T) where T.T == ImplicitlyUnwrappedOptional<Int> { } // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
func genericOptIUO<T : P>(_: T) where T.U == Optional<ImplicitlyUnwrappedOptional<Int>> {} // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}

func testClosure() -> Int {
  return {
    (i: ImplicitlyUnwrappedOptional<Int>) // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{9-37=}}{{40-40=!}}{{40-41=}}
     -> ImplicitlyUnwrappedOptional<Int> in // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
    return i
  }(1)
}

_ = Array<Int!>() // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
_ = [Int!]() // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
_ = Optional<Int!>(nil) // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
_ = Int!?(0) // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
_ = (
  Int!, // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  Float!, // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  String! // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
)(1, 2.0, "3")

struct Generic<T, U, C> {}
_ = Generic<Int!, // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
            Float!, // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
            String!>() // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
