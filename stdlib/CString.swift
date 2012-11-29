//===----------------------------------------------------------------------===//
// CString Type
//===----------------------------------------------------------------------===//

// XXX FIXME: we need a clean memory management story here

func [asmname="strdup"] strdup(arg : UnsafePointer<UInt8>) -> UnsafePointer<UInt8>
func [asmname="free"] free(arg : UnsafePointer<UInt8>)

struct CString {
  var cstr : UnsafePointer<UInt8>

  static func convertFromStringLiteral(val : Builtin.RawPointer,
                                       byteSize : Builtin.Int64) -> CString {
    var tmp : CString
    tmp.cstr.value = val
    return tmp
  }

  static func convertFromASCIIStringLiteral(val : Builtin.RawPointer,
                                       byteSize : Builtin.Int64) -> CString {
    var tmp : CString
    tmp.cstr.value = val
    return tmp
  }

  constructor(string : String) {
    cstr = strdup(string.str_value.getRawCString())
  }

  func done() {
    free(cstr)
  }

  func replPrint() {
    print('"')
    var tmp = 0
    while cstr[tmp] != 0 {
      var c = Char(UInt32(cstr[tmp]))
      c.replPrintCharBody()
      tmp += 1
    }
    print('"')
  }
}

extension String {
  func [conversion] __conversion() -> CString {
    var tmp : CString
    tmp.cstr = strdup(str_value.getRawCString())
    return tmp
  }
}
