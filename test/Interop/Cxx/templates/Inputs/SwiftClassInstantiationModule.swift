import ClassTemplateForSwiftModule

public func makeWrappedMagicNumber() -> MagicWrapper<IntWrapper> {
  let t = IntWrapper(value: 42)
  return MagicWrapper<IntWrapper>(t: t)
}

public func readWrappedMagicNumber(_ i: inout MagicWrapper<IntWrapper>) -> CInt {
  return i.getValuePlusArg(13)
}
