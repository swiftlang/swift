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
    return .someOptVar // expected-error 2 {{value of optional type 'Foo' not unwrapped; did you mean to use '!' or '?'?}} {{23-23=!}}
  // TODO
  //case ():
  //  return .someOptVar!
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someFunc()
  case (): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    return .someOptFunc() // expected-error{{}} {{26-26=!}}
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

