
struct PyGenObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var gi_frame: UnsafeMutablePointer<_frame>!
  var gi_running: Int32
  var gi_code: UnsafeMutablePointer<PyObject>!
  var gi_weakreflist: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, gi_frame gi_frame: UnsafeMutablePointer<_frame>!, gi_running gi_running: Int32, gi_code gi_code: UnsafeMutablePointer<PyObject>!, gi_weakreflist gi_weakreflist: UnsafeMutablePointer<PyObject>!)
}
var PyGen_Type: PyTypeObject
@discardableResult
func PyGen_New(_ _: UnsafeMutablePointer<_frame>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyGen_NeedsFinalizing(_ _: UnsafeMutablePointer<PyGenObject>!) -> Int32
