
struct PyFileObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var f_fp: UnsafeMutablePointer<FILE>!
  var f_name: UnsafeMutablePointer<PyObject>!
  var f_mode: UnsafeMutablePointer<PyObject>!
  var f_close: (@convention(c) (UnsafeMutablePointer<FILE>!) -> Int32)!
  var f_softspace: Int32
  var f_binary: Int32
  var f_buf: UnsafeMutablePointer<Int8>!
  var f_bufend: UnsafeMutablePointer<Int8>!
  var f_bufptr: UnsafeMutablePointer<Int8>!
  var f_setbuf: UnsafeMutablePointer<Int8>!
  var f_univ_newline: Int32
  var f_newlinetypes: Int32
  var f_skipnextlf: Int32
  var f_encoding: UnsafeMutablePointer<PyObject>!
  var f_errors: UnsafeMutablePointer<PyObject>!
  var weakreflist: UnsafeMutablePointer<PyObject>!
  var unlocked_count: Int32
  var readable: Int32
  var writable: Int32
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, f_fp f_fp: UnsafeMutablePointer<FILE>!, f_name f_name: UnsafeMutablePointer<PyObject>!, f_mode f_mode: UnsafeMutablePointer<PyObject>!, f_close f_close: (@convention(c) (UnsafeMutablePointer<FILE>!) -> Int32)!, f_softspace f_softspace: Int32, f_binary f_binary: Int32, f_buf f_buf: UnsafeMutablePointer<Int8>!, f_bufend f_bufend: UnsafeMutablePointer<Int8>!, f_bufptr f_bufptr: UnsafeMutablePointer<Int8>!, f_setbuf f_setbuf: UnsafeMutablePointer<Int8>!, f_univ_newline f_univ_newline: Int32, f_newlinetypes f_newlinetypes: Int32, f_skipnextlf f_skipnextlf: Int32, f_encoding f_encoding: UnsafeMutablePointer<PyObject>!, f_errors f_errors: UnsafeMutablePointer<PyObject>!, weakreflist weakreflist: UnsafeMutablePointer<PyObject>!, unlocked_count unlocked_count: Int32, readable readable: Int32, writable writable: Int32)
}
var PyFile_Type: PyTypeObject
@discardableResult
func PyFile_FromString(_ _: UnsafeMutablePointer<Int8>!, _ _: UnsafeMutablePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
func PyFile_SetBufSize(_ _: UnsafeMutablePointer<PyObject>!, _ _: Int32)
@discardableResult
func PyFile_SetEncoding(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!) -> Int32
@discardableResult
func PyFile_SetEncodingAndErrors(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!, _ errors: UnsafeMutablePointer<Int8>!) -> Int32
@discardableResult
func PyFile_FromFile(_ _: UnsafeMutablePointer<FILE>!, _ _: UnsafeMutablePointer<Int8>!, _ _: UnsafeMutablePointer<Int8>!, _ _: (@convention(c) (UnsafeMutablePointer<FILE>!) -> Int32)!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyFile_AsFile(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<FILE>!
func PyFile_IncUseCount(_ _: UnsafeMutablePointer<PyFileObject>!)
func PyFile_DecUseCount(_ _: UnsafeMutablePointer<PyFileObject>!)
@discardableResult
func PyFile_Name(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyFile_GetLine(_ _: UnsafeMutablePointer<PyObject>!, _ _: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyFile_WriteObject(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: Int32) -> Int32
@discardableResult
func PyFile_SoftSpace(_ _: UnsafeMutablePointer<PyObject>!, _ _: Int32) -> Int32
@discardableResult
func PyFile_WriteString(_ _: UnsafePointer<Int8>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyObject_AsFileDescriptor(_ _: UnsafeMutablePointer<PyObject>!) -> Int32
var Py_FileSystemDefaultEncoding: UnsafePointer<Int8>!
var PY_STDIOTEXTMODE: String { get }
@discardableResult
func Py_UniversalNewlineFgets(_ _: UnsafeMutablePointer<Int8>!, _ _: Int32, _ _: UnsafeMutablePointer<FILE>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func Py_UniversalNewlineFread(_ _: UnsafeMutablePointer<Int8>!, _ _: Int, _ _: UnsafeMutablePointer<FILE>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int
@discardableResult
func _PyFile_SanitizeMode(_ mode: UnsafeMutablePointer<Int8>!) -> Int32
