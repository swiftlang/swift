
struct PyIntObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var ob_ival: Int
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, ob_ival ob_ival: Int)
}
var PyInt_Type: PyTypeObject
@discardableResult
func PyInt_FromString(_ _: UnsafeMutablePointer<Int8>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!, _ _: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyInt_FromUnicode(_ _: UnsafeMutablePointer<Py_UNICODE>!, _ _: Py_ssize_t, _ _: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyInt_FromLong(_ _: Int) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyInt_FromSize_t(_ _: Int) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyInt_FromSsize_t(_ _: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyInt_AsLong(_ _: UnsafeMutablePointer<PyObject>!) -> Int
@discardableResult
func PyInt_AsSsize_t(_ _: UnsafeMutablePointer<PyObject>!) -> Py_ssize_t
@discardableResult
func _PyInt_AsInt(_ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyInt_AsUnsignedLongMask(_ _: UnsafeMutablePointer<PyObject>!) -> UInt
@discardableResult
func PyInt_AsUnsignedLongLongMask(_ _: UnsafeMutablePointer<PyObject>!) -> UInt64
@discardableResult
func PyInt_GetMax() -> Int
@discardableResult
func PyOS_strtoul(_ _: UnsafeMutablePointer<Int8>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!, _ _: Int32) -> UInt
@discardableResult
func PyOS_strtol(_ _: UnsafeMutablePointer<Int8>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!, _ _: Int32) -> Int
@discardableResult
func PyInt_ClearFreeList() -> Int32
@discardableResult
func _PyInt_Format(_ v: UnsafeMutablePointer<PyIntObject>!, _ base: Int32, _ newstyle: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyInt_FormatAdvanced(_ obj: UnsafeMutablePointer<PyObject>!, _ format_spec: UnsafeMutablePointer<Int8>!, _ format_spec_len: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
