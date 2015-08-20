// RUN: %target-parse-verify-swift

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
    return .Some(.someVar)
  case ():
    return .None
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
    return .Some(.someVar)
  case ():
    return .None
  }
}

// Favor the outermost type if the member appears at multiple levels of
// unwrapping.
func nestedOptContext() -> Foo!? {
  return .None
}

