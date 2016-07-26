
typealias PyLongObject = _longobject
var PyLong_Type: PyTypeObject
@discardableResult
func PyLong_FromLong(_ _: Int) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyLong_FromUnsignedLong(_ _: UInt) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyLong_FromDouble(_ _: Double) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyLong_FromSize_t(_ _: Int) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyLong_FromSsize_t(_ _: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyLong_AsLong(_ _: UnsafeMutablePointer<PyObject>!) -> Int
@discardableResult
func PyLong_AsLongAndOverflow(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<Int32>!) -> Int
@discardableResult
func PyLong_AsUnsignedLong(_ _: UnsafeMutablePointer<PyObject>!) -> UInt
@discardableResult
func PyLong_AsUnsignedLongMask(_ _: UnsafeMutablePointer<PyObject>!) -> UInt
@discardableResult
func PyLong_AsSsize_t(_ _: UnsafeMutablePointer<PyObject>!) -> Py_ssize_t
@discardableResult
func _PyLong_AsInt(_ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyLong_GetInfo() -> UnsafeMutablePointer<PyObject>!
var _PyLong_DigitValue: (Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
@discardableResult
func _PyLong_Frexp(_ a: UnsafeMutablePointer<PyLongObject>!, _ e: UnsafeMutablePointer<Py_ssize_t>!) -> Double
@discardableResult
func PyLong_AsDouble(_ _: UnsafeMutablePointer<PyObject>!) -> Double
@discardableResult
func PyLong_FromVoidPtr(_ _: UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyLong_AsVoidPtr(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<Void>!
@discardableResult
func PyLong_FromLongLong(_ _: Int64) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyLong_FromUnsignedLongLong(_ _: UInt64) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyLong_AsLongLong(_ _: UnsafeMutablePointer<PyObject>!) -> Int64
@discardableResult
func PyLong_AsUnsignedLongLong(_ _: UnsafeMutablePointer<PyObject>!) -> UInt64
@discardableResult
func PyLong_AsUnsignedLongLongMask(_ _: UnsafeMutablePointer<PyObject>!) -> UInt64
@discardableResult
func PyLong_AsLongLongAndOverflow(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<Int32>!) -> Int64
@discardableResult
func PyLong_FromString(_ _: UnsafeMutablePointer<Int8>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!, _ _: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyLong_FromUnicode(_ _: UnsafeMutablePointer<Py_UNICODE>!, _ _: Py_ssize_t, _ _: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyLong_Sign(_ v: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func _PyLong_NumBits(_ v: UnsafeMutablePointer<PyObject>!) -> Int
@discardableResult
func _PyLong_FromByteArray(_ bytes: UnsafePointer<UInt8>!, _ n: Int, _ little_endian: Int32, _ is_signed: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyLong_AsByteArray(_ v: UnsafeMutablePointer<PyLongObject>!, _ bytes: UnsafeMutablePointer<UInt8>!, _ n: Int, _ little_endian: Int32, _ is_signed: Int32) -> Int32
@discardableResult
func _PyLong_Format(_ aa: UnsafeMutablePointer<PyObject>!, _ base: Int32, _ addL: Int32, _ newstyle: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyLong_FormatAdvanced(_ obj: UnsafeMutablePointer<PyObject>!, _ format_spec: UnsafeMutablePointer<Int8>!, _ format_spec_len: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
