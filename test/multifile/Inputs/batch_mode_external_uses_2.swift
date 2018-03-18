#if FIELD2
public func use_extern_struct_field_2(e : extern_struct) -> Int32 {
  return e.field
}
#endif

#if RAWREP2
public func take_rawrep_2<T: RawRepresentable>(v: T) -> UInt32 where T.RawValue == UInt32 {
  return v.rawValue
}
public func use_extern_rawrep_2(e: extern_enum) -> UInt32 {
  return take_rawrep_2(v: e)
}
#endif

#if FUNC2
public func use_func_2() -> Int32 {
  return extern_inline_function(1, 2)
}
#endif
