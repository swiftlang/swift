
struct PyByteArrayObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var ob_size: Py_ssize_t
  var ob_exports: Int32
  var ob_alloc: Py_ssize_t
  var ob_bytes: UnsafeMutablePointer<Int8>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, ob_size ob_size: Py_ssize_t, ob_exports ob_exports: Int32, ob_alloc ob_alloc: Py_ssize_t, ob_bytes ob_bytes: UnsafeMutablePointer<Int8>!)
}
var PyByteArray_Type: PyTypeObject
var PyByteArrayIter_Type: PyTypeObject
@discardableResult
func PyByteArray_FromObject(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyByteArray_Concat(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyByteArray_FromStringAndSize(_ _: UnsafePointer<Int8>!, _ _: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyByteArray_Size(_ _: UnsafeMutablePointer<PyObject>!) -> Py_ssize_t
@discardableResult
func PyByteArray_AsString(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func PyByteArray_Resize(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t) -> Int32
