#if FIELD1
public func use_extern_struct_field_1(e: extern_struct) -> Int32 {
  return e.field
}
#endif

#if RAWREP1
public func take_rawrep_1<T: RawRepresentable>(v: T) -> UInt32 where T.RawValue == UInt32 {
  return v.rawValue
}
public func use_extern_rawrep_1(e: extern_enum) -> UInt32 {
  return take_rawrep_1(v: e)
}
#endif

#if FUNC1
public func use_func_1() -> Int32 {
  return extern_inline_function(1, 2)
}
#endif
