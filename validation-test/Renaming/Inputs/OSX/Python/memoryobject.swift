
var PyMemoryView_Type: PyTypeObject
@discardableResult
func PyMemoryView_GetContiguous(_ base: UnsafeMutablePointer<PyObject>!, _ buffertype: Int32, _ fort: Int8) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyMemoryView_FromObject(_ base: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyMemoryView_FromBuffer(_ info: UnsafeMutablePointer<Py_buffer>!) -> UnsafeMutablePointer<PyObject>!
struct PyMemoryViewObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var base: UnsafeMutablePointer<PyObject>!
  var view: Py_buffer
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, base base: UnsafeMutablePointer<PyObject>!, view view: Py_buffer)
}
