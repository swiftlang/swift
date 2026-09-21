// A transparent function, so the mandatory inliner pulls its SIL body into the
// client even at -Onone. The body calls a C function with a pass_object_size
// parameter, so the callee's lowered type carries the parameter flags, and the
// client has to deserialize them to lay the call out correctly.
@_transparent
public func callPassObject(_ p: UnsafeMutablePointer<CInt>) {
  pos_max(p)
}
