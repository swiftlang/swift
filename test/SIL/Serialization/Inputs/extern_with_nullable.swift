#if USE_EXTERN
@_extern(c, "takes_a_void_pointer")
public func takes_a_void_pointer(_ pointer: UnsafeRawPointer?)
#elseif USE_C_MODULE
import CTakesPtrNullable
#else
#error("Not configured")
#endif

@_alwaysEmitIntoClient
public func callWithNullable() {
  takes_a_void_pointer(nil)
}
