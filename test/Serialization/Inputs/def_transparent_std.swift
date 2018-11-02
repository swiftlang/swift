public class C {}

@_transparent public func foo(x: Builtin.Int1, y: Builtin.Int1) -> Builtin.Int1 {
  return Builtin.cmp_eq_Int1(x, y)
}

@_transparent public func destroy_obj(x: Builtin.RawPointer) {
  return Builtin.destroy(Builtin.NativeObject.self, x)
}

@_transparent public func assign_tuple(x: (Builtin.Int64, Builtin.NativeObject),
                               y: Builtin.RawPointer) {
  Builtin.assign(x, y)
}

@_transparent public func class_to_native_object(c: C) -> Builtin.NativeObject {
  return Builtin.castToNativeObject(c)
}

@_transparent public func class_from_native_object(p: Builtin.NativeObject) -> C {
  return Builtin.castFromNativeObject(p)
}

@_transparent public func class_to_raw_pointer(c: C) -> Builtin.RawPointer {
  return Builtin.bridgeToRawPointer(c)
}

@_transparent public func class_from_raw_pointer(p: Builtin.RawPointer) -> C {
  return Builtin.bridgeFromRawPointer(p)
}

@_transparent public func gep32(p: Builtin.RawPointer, i: Builtin.Int32) -> Builtin.RawPointer {
  return Builtin.gepRaw_Int32(p, i)
}
