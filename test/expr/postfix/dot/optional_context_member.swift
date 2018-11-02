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
    // FIXME: Customize this diagnostic for the optional case.
    return .someOptVar // expected-error 1 {{member 'someOptVar' in 'Foo' produces result of type 'Foo?', but context expects 'Foo'}}
  // TODO
  //case ():
  //  return .someOptVar!
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someFunc()
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someOptFunc() // expected-error{{member 'someOptFunc' in 'Foo' produces result of type 'Foo?', but context expects 'Foo'}}
  // TODO
  //case ():
  //  return .someOptFunc()!
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
  static var palomino: Horse { get }
}

func rideAHorse(_ horse: Horse?) {}

rideAHorse(.palomino)
// expected-error@-1 {{static member 'palomino' cannot be used on protocol metatype 'Horse.Protocol'}}

// FIXME: This should work if the static member is part of a class though
class Donkey {
  static var mule: Donkey & Horse { while true {} }
}

func rideAMule(_ mule: (Horse & Donkey)?) {}

rideAMule(.mule)
// expected-error@-1 {{static member 'mule' cannot be used on protocol metatype '(Donkey & Horse).Protocol'}}
