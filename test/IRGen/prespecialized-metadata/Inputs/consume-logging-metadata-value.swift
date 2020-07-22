func unsafePointerToMetadata<T>(of ty: T.Type) -> UnsafePointer<T.Type> {
  unsafeBitCast(ty, to: UnsafePointer<T.Type>.self)
}

@inline(never)
func consume<T>(_ value: T) {
  withExtendedLifetime(value) {
    print(unsafePointerToMetadata(of: T.self), value)
  }
}

@inline(never)
func consumeType<T>(_ type: T.Type, line: UInt = #line) {
  withExtendedLifetime(type) {
    print(unsafePointerToMetadata(of: T.self), "@", line)
  }
}
