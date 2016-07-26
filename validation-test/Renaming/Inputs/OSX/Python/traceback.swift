
struct _traceback {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var tb_next: UnsafeMutablePointer<_traceback>!
  var tb_frame: UnsafeMutablePointer<_frame>!
  var tb_lasti: Int32
  var tb_lineno: Int32
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, tb_next tb_next: UnsafeMutablePointer<_traceback>!, tb_frame tb_frame: UnsafeMutablePointer<_frame>!, tb_lasti tb_lasti: Int32, tb_lineno tb_lineno: Int32)
}
typealias PyTracebackObject = _traceback
@discardableResult
func PyTraceBack_Here(_ _: UnsafeMutablePointer<_frame>!) -> Int32
@discardableResult
func PyTraceBack_Print(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func _Py_DisplaySourceLine(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!, _ _: Int32, _ _: Int32) -> Int32
var PyTraceBack_Type: PyTypeObject
