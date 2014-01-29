@asmname="swift_makeUnsafeNil" func _makeUnsafeNil() -> AnyObject

struct _Nil {
  @conversion func __conversion() -> AnyObject {
    return _makeUnsafeNil()
  }
}

/// A null pointer constant.
var nil : _Nil {
  return _Nil()
}
