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
  case ():
    return .someOptVar // expected-error 2 {{value of optional type 'Foo' not unwrapped; did you mean to use '!' or '?'?}} {{23-23=!}}
  // TODO
  //case ():
  //  return .someOptVar!
  case ():
    return .someFunc()
  case ():
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
  case ():
    return .someOptVar
  case ():
    return .someFunc()
  case ():
    return .someOptFunc()
  case ():
    return .some(.someVar)
  case ():
    return .none
  }
}

func iuoContext() -> Foo! {
  switch () {
  case ():
    return .someVar
  case ():
    return .someOptVar
  case ():
    return .someFunc()
  case ():
    return .someOptFunc()
  case ():
    return .some(.someVar)
  case ():
    return .none
  }
}

// Favor the outermost type if the member appears at multiple levels of
// unwrapping.
func nestedOptContext() -> Foo?? {
  return .none
}

