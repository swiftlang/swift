import ClassTemplateForSwiftModule

public func makeWrappedMagicNumber() -> MagicWrapperSpec {
  let t = IntWrapper(value: 42)
  return MagicWrapper<IntWrapper>(t: t)
}

public func readWrappedMagicNumber(_ i: inout MagicWrapperSpec) -> CInt {
  return i.getValuePlusArg(13)
}
