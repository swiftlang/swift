
typealias Py_UCS4 = UInt32
typealias Py_UNICODE = UInt16
struct PyUnicodeObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var length: Py_ssize_t
  var str: UnsafeMutablePointer<Py_UNICODE>!
  var hash: Int
  var defenc: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, length length: Py_ssize_t, str str: UnsafeMutablePointer<Py_UNICODE>!, hash hash: Int, defenc defenc: UnsafeMutablePointer<PyObject>!)
}
var PyUnicode_Type: PyTypeObject
@discardableResult
func PyUnicodeUCS2_FromUnicode(_ u: UnsafePointer<Py_UNICODE>!, _ size: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_FromStringAndSize(_ u: UnsafePointer<Int8>!, _ size: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_FromString(_ u: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsUnicode(_ unicode: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<Py_UNICODE>!
@discardableResult
func PyUnicodeUCS2_GetSize(_ unicode: UnsafeMutablePointer<PyObject>!) -> Py_ssize_t
@discardableResult
func PyUnicodeUCS2_GetMax() -> Py_UNICODE
@discardableResult
func PyUnicodeUCS2_Resize(_ unicode: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ length: Py_ssize_t) -> Int32
@discardableResult
func PyUnicodeUCS2_FromEncodedObject(_ obj: UnsafeMutablePointer<PyObject>!, _ encoding: UnsafePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_FromObject(_ obj: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_FromFormatV(_ _: UnsafePointer<Int8>!, _ _: CVaListPointer) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyUnicode_FormatAdvanced(_ obj: UnsafeMutablePointer<PyObject>!, _ format_spec: UnsafeMutablePointer<Py_UNICODE>!, _ format_spec_len: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_FromWideChar(_ w: UnsafePointer<wchar_t>!, _ size: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsWideChar(_ unicode: UnsafeMutablePointer<PyUnicodeObject>!, _ w: UnsafeMutablePointer<wchar_t>!, _ size: Py_ssize_t) -> Py_ssize_t
@discardableResult
func PyUnicodeUCS2_FromOrdinal(_ ordinal: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_ClearFreelist() -> Int32
@discardableResult
func _PyUnicodeUCS2_AsDefaultEncodedString(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_GetDefaultEncoding() -> UnsafePointer<Int8>!
@discardableResult
func PyUnicodeUCS2_SetDefaultEncoding(_ encoding: UnsafePointer<Int8>!) -> Int32
@discardableResult
func PyUnicodeUCS2_Decode(_ s: UnsafePointer<Int8>!, _ size: Py_ssize_t, _ encoding: UnsafePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_Encode(_ s: UnsafePointer<Py_UNICODE>!, _ size: Py_ssize_t, _ encoding: UnsafePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsEncodedObject(_ unicode: UnsafeMutablePointer<PyObject>!, _ encoding: UnsafePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsEncodedString(_ unicode: UnsafeMutablePointer<PyObject>!, _ encoding: UnsafePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicode_BuildEncodingMap(_ string: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicode_DecodeUTF7(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicode_DecodeUTF7Stateful(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!, _ consumed: UnsafeMutablePointer<Py_ssize_t>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicode_EncodeUTF7(_ data: UnsafePointer<Py_UNICODE>!, _ length: Py_ssize_t, _ base64SetO: Int32, _ base64WhiteSpace: Int32, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_DecodeUTF8(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_DecodeUTF8Stateful(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!, _ consumed: UnsafeMutablePointer<Py_ssize_t>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsUTF8String(_ unicode: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_EncodeUTF8(_ data: UnsafePointer<Py_UNICODE>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_DecodeUTF32(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!, _ byteorder: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_DecodeUTF32Stateful(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!, _ byteorder: UnsafeMutablePointer<Int32>!, _ consumed: UnsafeMutablePointer<Py_ssize_t>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsUTF32String(_ unicode: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_EncodeUTF32(_ data: UnsafePointer<Py_UNICODE>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!, _ byteorder: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_DecodeUTF16(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!, _ byteorder: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_DecodeUTF16Stateful(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!, _ byteorder: UnsafeMutablePointer<Int32>!, _ consumed: UnsafeMutablePointer<Py_ssize_t>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsUTF16String(_ unicode: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_EncodeUTF16(_ data: UnsafePointer<Py_UNICODE>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!, _ byteorder: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_DecodeUnicodeEscape(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsUnicodeEscapeString(_ unicode: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_EncodeUnicodeEscape(_ data: UnsafePointer<Py_UNICODE>!, _ length: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_DecodeRawUnicodeEscape(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsRawUnicodeEscapeString(_ unicode: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_EncodeRawUnicodeEscape(_ data: UnsafePointer<Py_UNICODE>!, _ length: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyUnicode_DecodeUnicodeInternal(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_DecodeLatin1(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsLatin1String(_ unicode: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_EncodeLatin1(_ data: UnsafePointer<Py_UNICODE>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_DecodeASCII(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsASCIIString(_ unicode: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_EncodeASCII(_ data: UnsafePointer<Py_UNICODE>!, _ length: Py_ssize_t, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_DecodeCharmap(_ string: UnsafePointer<Int8>!, _ length: Py_ssize_t, _ mapping: UnsafeMutablePointer<PyObject>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_AsCharmapString(_ unicode: UnsafeMutablePointer<PyObject>!, _ mapping: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_EncodeCharmap(_ data: UnsafePointer<Py_UNICODE>!, _ length: Py_ssize_t, _ mapping: UnsafeMutablePointer<PyObject>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_TranslateCharmap(_ data: UnsafePointer<Py_UNICODE>!, _ length: Py_ssize_t, _ table: UnsafeMutablePointer<PyObject>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_EncodeDecimal(_ s: UnsafeMutablePointer<Py_UNICODE>!, _ length: Py_ssize_t, _ output: UnsafeMutablePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> Int32
@discardableResult
func PyUnicodeUCS2_Concat(_ left: UnsafeMutablePointer<PyObject>!, _ right: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_Split(_ s: UnsafeMutablePointer<PyObject>!, _ sep: UnsafeMutablePointer<PyObject>!, _ maxsplit: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_Splitlines(_ s: UnsafeMutablePointer<PyObject>!, _ keepends: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_Partition(_ s: UnsafeMutablePointer<PyObject>!, _ sep: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_RPartition(_ s: UnsafeMutablePointer<PyObject>!, _ sep: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_RSplit(_ s: UnsafeMutablePointer<PyObject>!, _ sep: UnsafeMutablePointer<PyObject>!, _ maxsplit: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_Translate(_ str: UnsafeMutablePointer<PyObject>!, _ table: UnsafeMutablePointer<PyObject>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_Join(_ separator: UnsafeMutablePointer<PyObject>!, _ seq: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_Tailmatch(_ str: UnsafeMutablePointer<PyObject>!, _ substr: UnsafeMutablePointer<PyObject>!, _ start: Py_ssize_t, _ end: Py_ssize_t, _ direction: Int32) -> Py_ssize_t
@discardableResult
func PyUnicodeUCS2_Find(_ str: UnsafeMutablePointer<PyObject>!, _ substr: UnsafeMutablePointer<PyObject>!, _ start: Py_ssize_t, _ end: Py_ssize_t, _ direction: Int32) -> Py_ssize_t
@discardableResult
func PyUnicodeUCS2_Count(_ str: UnsafeMutablePointer<PyObject>!, _ substr: UnsafeMutablePointer<PyObject>!, _ start: Py_ssize_t, _ end: Py_ssize_t) -> Py_ssize_t
@discardableResult
func PyUnicodeUCS2_Replace(_ str: UnsafeMutablePointer<PyObject>!, _ substr: UnsafeMutablePointer<PyObject>!, _ replstr: UnsafeMutablePointer<PyObject>!, _ maxcount: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_Compare(_ left: UnsafeMutablePointer<PyObject>!, _ right: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyUnicodeUCS2_RichCompare(_ left: UnsafeMutablePointer<PyObject>!, _ right: UnsafeMutablePointer<PyObject>!, _ op: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_Format(_ format: UnsafeMutablePointer<PyObject>!, _ args: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeUCS2_Contains(_ container: UnsafeMutablePointer<PyObject>!, _ element: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func _PyUnicode_XStrip(_ self: UnsafeMutablePointer<PyUnicodeObject>!, _ striptype: Int32, _ sepobj: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyUnicodeUCS2_IsLowercase(_ ch: Py_UNICODE) -> Int32
@discardableResult
func _PyUnicodeUCS2_IsUppercase(_ ch: Py_UNICODE) -> Int32
@discardableResult
func _PyUnicodeUCS2_IsTitlecase(_ ch: Py_UNICODE) -> Int32
@discardableResult
func _PyUnicodeUCS2_IsWhitespace(_ ch: Py_UNICODE) -> Int32
@discardableResult
func _PyUnicodeUCS2_IsLinebreak(_ ch: Py_UNICODE) -> Int32
@discardableResult
func _PyUnicodeUCS2_ToLowercase(_ ch: Py_UNICODE) -> Py_UNICODE
@discardableResult
func _PyUnicodeUCS2_ToUppercase(_ ch: Py_UNICODE) -> Py_UNICODE
@discardableResult
func _PyUnicodeUCS2_ToTitlecase(_ ch: Py_UNICODE) -> Py_UNICODE
@discardableResult
func _PyUnicodeUCS2_ToDecimalDigit(_ ch: Py_UNICODE) -> Int32
@discardableResult
func _PyUnicodeUCS2_ToDigit(_ ch: Py_UNICODE) -> Int32
@discardableResult
func _PyUnicodeUCS2_ToNumeric(_ ch: Py_UNICODE) -> Double
@discardableResult
func _PyUnicodeUCS2_IsDecimalDigit(_ ch: Py_UNICODE) -> Int32
@discardableResult
func _PyUnicodeUCS2_IsDigit(_ ch: Py_UNICODE) -> Int32
@discardableResult
func _PyUnicodeUCS2_IsNumeric(_ ch: Py_UNICODE) -> Int32
@discardableResult
func _PyUnicodeUCS2_IsAlpha(_ ch: Py_UNICODE) -> Int32
