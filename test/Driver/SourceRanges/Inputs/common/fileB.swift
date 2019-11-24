struct Struct1InB { // used by A
  // asdf
  var instVar1 = 0
  var instVar2 = 0
  func watchMe<T: SignedInteger>(_: T) -> String {"SignedInteger"}
}
struct Struct2InB { // not used
  var instVar1 = 0
  var instVar2 = 0
}
