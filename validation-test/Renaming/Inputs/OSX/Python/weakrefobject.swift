
typealias PyWeakReference = _PyWeakReference
struct _PyWeakReference {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var wr_object: UnsafeMutablePointer<PyObject>!
  var wr_callback: UnsafeMutablePointer<PyObject>!
  var hash: Int
  var wr_prev: UnsafeMutablePointer<PyWeakReference>!
  var wr_next: UnsafeMutablePointer<PyWeakReference>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, wr_object wr_object: UnsafeMutablePointer<PyObject>!, wr_callback wr_callback: UnsafeMutablePointer<PyObject>!, hash hash: Int, wr_prev wr_prev: UnsafeMutablePointer<PyWeakReference>!, wr_next wr_next: UnsafeMutablePointer<PyWeakReference>!)
}
var _PyWeakref_RefType: PyTypeObject
var _PyWeakref_ProxyType: PyTypeObject
var _PyWeakref_CallableProxyType: PyTypeObject
@discardableResult
func PyWeakref_NewRef(_ ob: UnsafeMutablePointer<PyObject>!, _ callback: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyWeakref_NewProxy(_ ob: UnsafeMutablePointer<PyObject>!, _ callback: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyWeakref_GetObject(_ ref: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyWeakref_GetWeakrefCount(_ head: UnsafeMutablePointer<PyWeakReference>!) -> Py_ssize_t
func _PyWeakref_ClearRef(_ self: UnsafeMutablePointer<PyWeakReference>!)
