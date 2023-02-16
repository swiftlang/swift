import Swift

@_silgen_name("printGenericType")
public func printGenericType<T>(_ t: T.Type) {
  print(T.self)
}

@_silgen_name("printGenericTypeAndWord")
public func printGenericType<T>(_ t: T.Type, _ w: Builtin.Word) {
  print(T.self, Int(w))
}

