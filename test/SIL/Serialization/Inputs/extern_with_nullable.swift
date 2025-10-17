@_extern(c, "takes_a_void_pointer")
public func takes_a_void_pointer(_ pointer: UnsafeRawPointer?)

@_alwaysEmitIntoClient
public func callWithNullable() {
  takes_a_void_pointer(nil)
}
