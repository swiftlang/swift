//===----------------------------------------------------------------------===//
// CString Type
//===----------------------------------------------------------------------===//

// XXX FIXME: we need a clean memory management story here

struct CString : BuiltinStringLiteralConvertible, StringLiteralConvertible{
  var _bytesPtr : UnsafePointer<UInt8>

  static func _convertFromBuiltinStringLiteral(value : Builtin.RawPointer,
                                               byteSize : Builtin.Int64,
                                               isASCII : Builtin.Int1) -> CString {
    return CString(UnsafePointer(value))
  }

  typealias StringLiteralType = CString
  static func convertFromStringLiteral(value : CString) -> CString {
    return value
  }

  func isNull() -> Bool {
    return _bytesPtr.isNull()
  }

  func replPrint() {
    print('"')
    var i = 0
    while _bytesPtr[i] != 0 {
      var c = Char(UInt32(_bytesPtr[i]))
      c.replPrintCharBody()
      i++
    }
    print('"')
  }
}

func [asmname="strlen"] _strlen(arg : CString) -> Word
func [asmname="strcpy"] _strcpy(dest : CString, src : CString) -> CString

extension String {
  /// Creates a new String by copying the null-terminated data referenced by
  /// a CString.
  static func fromCString(cs : CString) -> String {
    var len = Int(_strlen(cs))
    var buf = StringByteData.getNew(len + 1)
    _strcpy(CString(buf.base), cs)
    buf.length = len
    buf.setASCII(false)
    buf.setCString(true)
    return String(buf)
  }

  static func fromCString(up : UnsafePointer<CChar>) -> String {
    return fromCString(CString(UnsafePointer<UInt8>(up)))
  }

  func _toCString() -> CString {
    assert(str_value.isCString())
    return CString(str_value.base)
  }

  func _toUnsafePointer() -> UnsafePointer<CChar> {
    assert(str_value.isCString())
    return UnsafePointer(str_value.base)
  }
}

extension LifetimeManager {
  /// \brief Returns an equivalent CString; lifetime of the underlying storage
  /// is extended until the call to \c release().
  func getCString(s : String) -> CString {
    s._makeNulTerminated()
    this.put(s.str_value.owner)
    return s._toCString()
  }

  /// \brief Returns an equivalent UnsafePointer<CChar>; lifetime of the
  /// underlying storage is extended until the call to \c release().
  func getCString(s : String) -> UnsafePointer<CChar> {
    s._makeNulTerminated()
    this.put(s.str_value.owner)
    return s._toUnsafePointer()
  }
}

