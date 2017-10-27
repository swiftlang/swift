// RUN: %target-typecheck-verify-swift

let _: ImplicitlyUnwrappedOptional<Int> = 1 // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{8-36=}}{{39-39=!}}{{39-40=}}
let _: ImplicitlyUnwrappedOptional = 1 // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated}}

extension ImplicitlyUnwrappedOptional {} // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated}}

func function(
  _: ImplicitlyUnwrappedOptional<Int> // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{6-34=}}{{37-37=!}}{{37-38=}}
) -> ImplicitlyUnwrappedOptional<Int> { // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{6-34=}}{{37-37=!}}{{37-38=}}
  return 1
}

func genericFunction<T>(
  iuo: ImplicitlyUnwrappedOptional<T> // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{8-36=}}{{37-37=!}}{{37-38=}}
) -> ImplicitlyUnwrappedOptional<T> { // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{6-34=}}{{35-35=!}}{{35-36=}}
  return iuo
}

protocol P {
  associatedtype T
  associatedtype U
}

struct S : P {
  typealias T = ImplicitlyUnwrappedOptional<Int> // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated}}
  typealias U = Optional<ImplicitlyUnwrappedOptional<Int>> // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated}}

  subscript (
    index: ImplicitlyUnwrappedOptional<Int> // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{12-40=}}{{43-43=!}}{{43-44=}}
  )     -> ImplicitlyUnwrappedOptional<Int> { // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{12-40=}}{{43-43=!}}{{43-44=}}
    return index
  }
}

func generic<T : P>(_: T) where T.T == ImplicitlyUnwrappedOptional<Int> { } // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated}}
func genericOptIUO<T : P>(_: T) where T.U == Optional<ImplicitlyUnwrappedOptional<Int>> {} // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated}}

func testClosure() -> Int {
  return {
    (i: ImplicitlyUnwrappedOptional<Int>) // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{9-37=}}{{40-40=!}}{{40-41=}}
     -> ImplicitlyUnwrappedOptional<Int> in // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated}}
    return i
  }(1)
}
