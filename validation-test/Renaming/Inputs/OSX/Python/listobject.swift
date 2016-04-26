
struct PyListObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var ob_size: Py_ssize_t
  var ob_item: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!
  var allocated: Py_ssize_t
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, ob_size ob_size: Py_ssize_t, ob_item ob_item: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, allocated allocated: Py_ssize_t)
}
var PyList_Type: PyTypeObject
@discardableResult
func PyList_New(_ size: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyList_Size(_ _: UnsafeMutablePointer<PyObject>!) -> Py_ssize_t
@discardableResult
func PyList_GetItem(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyList_SetItem(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyList_Insert(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyList_Append(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyList_GetSlice(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t, _ _: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyList_SetSlice(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t, _ _: Py_ssize_t, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyList_Sort(_ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyList_Reverse(_ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyList_AsTuple(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyList_Extend(_ _: UnsafeMutablePointer<PyListObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
