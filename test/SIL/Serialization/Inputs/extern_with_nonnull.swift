@_extern(c, "takes_a_void_pointer")
public func takes_a_void_pointer(_ pointer: UnsafeRawPointer)

@_alwaysEmitIntoClient
public func callWithNonNull() {
  let pointer = UnsafeMutablePointer<Int>.allocate(capacity: 1)
  takes_a_void_pointer(UnsafeRawPointer(pointer))
}
