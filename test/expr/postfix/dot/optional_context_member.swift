// RUN: %target-typecheck-verify-swift

struct Foo {
  static var someVar: Foo = Foo()
  static var someOptVar: Foo? = Foo()

  static func someFunc() -> Foo {}
  static func someOptFunc() -> Foo? {}
}

func nonOptContext() -> Foo {
  switch () {
  case ():
    return .someVar
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someOptVar
    // expected-error@-1 {{value of optional type 'Foo?' must be unwrapped to a value of type 'Foo'}}
    // expected-note@-2 {{coalesce}}
    // expected-note@-3 {{force-unwrap}}
  // TODO
  //case ():
  //  return .someOptVar!
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someFunc()
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someOptFunc()
    // expected-error@-1 {{value of optional type 'Foo?' must be unwrapped to a value of type 'Foo'}}
    // expected-note@-2 {{coalesce}}
    // expected-note@-3 {{force-unwrap}}
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someOptFunc()!
  }
}

func optContext() -> Foo? {
  switch () {
  case ():
    return .someVar
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someOptVar
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someFunc()
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someOptFunc()
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .some(.someVar)
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .none
  }
}

func iuoContext() -> Foo! {
  switch () {
  case ():
    return .someVar
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someOptVar
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someFunc()
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someOptFunc()
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .some(.someVar)
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .none
  }
}

// Favor the outermost type if the member appears at multiple levels of
// unwrapping.
func nestedOptContext() -> Foo?? {
  return .none
}

// <rdar://problem/35945827>

// This should diagnose instead of crashing in SILGen
protocol Horse {
  static var palomino: Horse { get } // expected-note {{'palomino' declared here}}
}

func rideAHorse(_ horse: Horse?) {}

rideAHorse(.palomino)
// expected-error@-1 {{cannot reference static property 'palomino' on 'Horse.Protocol' with non-conforming result type 'Horse'}}
// expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}

// FIXME: This should work if the static member is part of a class though
class Donkey {
  static var mule: Donkey & Horse { while true {} }
}

func rideAMule(_ mule: (Horse & Donkey)?) {}

rideAMule(.mule)
// expected-error@-1 {{static member 'mule' cannot be used on protocol metatype '(Donkey & Horse).Protocol'}}
