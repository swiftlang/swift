
var _Py_EllipsisObject: PyObject
struct PySliceObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var start: UnsafeMutablePointer<PyObject>!
  var stop: UnsafeMutablePointer<PyObject>!
  var step: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, start start: UnsafeMutablePointer<PyObject>!, stop stop: UnsafeMutablePointer<PyObject>!, step step: UnsafeMutablePointer<PyObject>!)
}
var PySlice_Type: PyTypeObject
var PyEllipsis_Type: PyTypeObject
@discardableResult
func PySlice_New(_ start: UnsafeMutablePointer<PyObject>!, _ stop: UnsafeMutablePointer<PyObject>!, _ step: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PySlice_FromIndices(_ start: Py_ssize_t, _ stop: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PySlice_GetIndices(_ r: UnsafeMutablePointer<PySliceObject>!, _ length: Py_ssize_t, _ start: UnsafeMutablePointer<Py_ssize_t>!, _ stop: UnsafeMutablePointer<Py_ssize_t>!, _ step: UnsafeMutablePointer<Py_ssize_t>!) -> Int32
@discardableResult
func PySlice_GetIndicesEx(_ r: UnsafeMutablePointer<PySliceObject>!, _ length: Py_ssize_t, _ start: UnsafeMutablePointer<Py_ssize_t>!, _ stop: UnsafeMutablePointer<Py_ssize_t>!, _ step: UnsafeMutablePointer<Py_ssize_t>!, _ slicelength: UnsafeMutablePointer<Py_ssize_t>!) -> Int32
