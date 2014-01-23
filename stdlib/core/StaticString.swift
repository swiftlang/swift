/// \brief An extremely simple string designed to represent something
/// "statically knowable".

// Implementation Note: Because StaticString is used in the
// implementation of assert() and fatal(), we keep it extremely close
// to the bare metal.  In particular, because we use only Builtin
// types, we are guaranteed that no assertions are involved in its
// construction.  This feature is crucial for preventing infinite
// recursion even in non-asserting cases.
struct StaticString : BuiltinStringLiteralConvertible, StringLiteralConvertible {
  var start: Builtin.RawPointer
  var byteCount: Builtin.Word
  var isASCII: Builtin.Int1

  init() {
    self = ""
  }
  
  init(start: Builtin.RawPointer, byteCount: Builtin.Word, isASCII: Builtin.Int1) {
    self.start = start
    self.byteCount = byteCount
    self.isASCII = isASCII
  }

  // FIXME: this is a hackaround for <rdar://problem/15888736> fatal()
  // can't take a StaticString?!
  init(nulTerminated: CString) {
    self.start = nulTerminated._bytesPtr.value
    self.byteCount = _strlen(nulTerminated).value
    self.isASCII = false.value
  }
  
  type func _convertFromBuiltinStringLiteral(
    start: Builtin.RawPointer, byteCount: Builtin.Word, isASCII: Builtin.Int1
  ) -> StaticString {
    return StaticString(start: start, byteCount: byteCount, isASCII: isASCII)
  }
  
  type func convertFromStringLiteral(value: StaticString) -> StaticString {
    return value
  }
}
