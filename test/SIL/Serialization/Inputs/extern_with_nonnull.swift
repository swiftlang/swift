#if USE_EXTERN
@_extern(c, "takes_a_void_pointer")
public func takes_a_void_pointer(_ pointer: UnsafeRawPointer)
#elseif USE_C_MODULE
import CTakesPtrNonNull
#else
#error("Not configured")
#endif

@_alwaysEmitIntoClient
public func callWithNonNull() {
  let pointer = UnsafeMutablePointer<Int>.allocate(capacity: 1)
  takes_a_void_pointer(UnsafeRawPointer(pointer))
}
