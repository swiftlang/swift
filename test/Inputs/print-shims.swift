@_silgen_name("printInt64")
public func printInt64(_ int: Int64) {
  print(int)
}

@_silgen_name("printInt32")
public func printInt32(_ int: Int32) {
  print(int)
}

@_silgen_name("printGeneric")
public func printGeneric<T>(_ t: T) {
  print(t)
}

@_silgen_name("printBool")
public func printBool(_ bool: Bool) {
  print(bool)
}

@_silgen_name("printAny")
public func printAny(_ any: Any) {
  print(any)
}

@_silgen_name("printGenericType")
public func printGenericType<T>(_ t: T.Type) {
  print(T.self)
}

