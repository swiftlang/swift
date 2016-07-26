
struct PyStringObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var ob_size: Py_ssize_t
  var ob_shash: Int
  var ob_sstate: Int32
  var ob_sval: (Int8)
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, ob_size ob_size: Py_ssize_t, ob_shash ob_shash: Int, ob_sstate ob_sstate: Int32, ob_sval ob_sval: (Int8))
}
var SSTATE_NOT_INTERNED: Int32 { get }
var SSTATE_INTERNED_MORTAL: Int32 { get }
var SSTATE_INTERNED_IMMORTAL: Int32 { get }
var PyBaseString_Type: PyTypeObject
var PyString_Type: PyTypeObject
@discardableResult
func PyString_FromStringAndSize(_ _: UnsafePointer<Int8>!, _ _: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyString_FromString(_ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyString_FromFormatV(_ _: UnsafePointer<Int8>!, _ _: CVaListPointer) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyString_Size(_ _: UnsafeMutablePointer<PyObject>!) -> Py_ssize_t
@discardableResult
func PyString_AsString(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func PyString_Repr(_ _: UnsafeMutablePointer<PyObject>!, _ _: Int32) -> UnsafeMutablePointer<PyObject>!
func PyString_Concat(_ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ _: UnsafeMutablePointer<PyObject>!)
func PyString_ConcatAndDel(_ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ _: UnsafeMutablePointer<PyObject>!)
@discardableResult
func _PyString_Resize(_ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ _: Py_ssize_t) -> Int32
@discardableResult
func _PyString_Eq(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyString_Format(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyString_FormatLong(_ _: UnsafeMutablePointer<PyObject>!, _ _: Int32, _ _: Int32, _ _: Int32, _ _: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!, _ _: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyString_DecodeEscape(_ _: UnsafePointer<Int8>!, _ _: Py_ssize_t, _ _: UnsafePointer<Int8>!, _ _: Py_ssize_t, _ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
func PyString_InternInPlace(_ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!)
func PyString_InternImmortal(_ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!)
@discardableResult
func PyString_InternFromString(_ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
func _Py_ReleaseInternedStrings()
@discardableResult
func _PyString_Join(_ sep: UnsafeMutablePointer<PyObject>!, _ x: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyString_Decode(_ s: UnsafePointer<Int8>!, _ size: Py_ssize_t, _ encoding: UnsafePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyString_Encode(_ s: UnsafePointer<Int8>!, _ size: Py_ssize_t, _ encoding: UnsafePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyString_AsEncodedObject(_ str: UnsafeMutablePointer<PyObject>!, _ encoding: UnsafePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyString_AsEncodedString(_ str: UnsafeMutablePointer<PyObject>!, _ encoding: UnsafePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyString_AsDecodedObject(_ str: UnsafeMutablePointer<PyObject>!, _ encoding: UnsafePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyString_AsDecodedString(_ str: UnsafeMutablePointer<PyObject>!, _ encoding: UnsafePointer<Int8>!, _ errors: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyString_AsStringAndSize(_ obj: UnsafeMutablePointer<PyObject>!, _ s: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!, _ len: UnsafeMutablePointer<Py_ssize_t>!) -> Int32
@discardableResult
func _PyString_InsertThousandsGroupingLocale(_ buffer: UnsafeMutablePointer<Int8>!, _ n_buffer: Py_ssize_t, _ digits: UnsafeMutablePointer<Int8>!, _ n_digits: Py_ssize_t, _ min_width: Py_ssize_t) -> Py_ssize_t
@discardableResult
func _PyString_InsertThousandsGrouping(_ buffer: UnsafeMutablePointer<Int8>!, _ n_buffer: Py_ssize_t, _ digits: UnsafeMutablePointer<Int8>!, _ n_digits: Py_ssize_t, _ min_width: Py_ssize_t, _ grouping: UnsafePointer<Int8>!, _ thousands_sep: UnsafePointer<Int8>!) -> Py_ssize_t
@discardableResult
func _PyBytes_FormatAdvanced(_ obj: UnsafeMutablePointer<PyObject>!, _ format_spec: UnsafeMutablePointer<Int8>!, _ format_spec_len: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
