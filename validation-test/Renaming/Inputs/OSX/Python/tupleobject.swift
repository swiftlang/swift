
struct PyTupleObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var ob_size: Py_ssize_t
  var ob_item: (UnsafeMutablePointer<PyObject>?)
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, ob_size ob_size: Py_ssize_t, ob_item ob_item: (UnsafeMutablePointer<PyObject>?))
}
var PyTuple_Type: PyTypeObject
@discardableResult
func PyTuple_New(_ size: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyTuple_Size(_ _: UnsafeMutablePointer<PyObject>!) -> Py_ssize_t
@discardableResult
func PyTuple_GetItem(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyTuple_SetItem(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyTuple_GetSlice(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t, _ _: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyTuple_Resize(_ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ _: Py_ssize_t) -> Int32
func _PyTuple_MaybeUntrack(_ _: UnsafeMutablePointer<PyObject>!)
@discardableResult
func PyTuple_ClearFreeList() -> Int32
