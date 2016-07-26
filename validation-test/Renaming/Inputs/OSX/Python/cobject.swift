
var PyCObject_Type: PyTypeObject
@discardableResult
func PyCObject_FromVoidPtr(_ cobj: UnsafeMutablePointer<Void>!, _ destruct: (@convention(c) (UnsafeMutablePointer<Void>!) -> Void)!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyCObject_FromVoidPtrAndDesc(_ cobj: UnsafeMutablePointer<Void>!, _ desc: UnsafeMutablePointer<Void>!, _ destruct: (@convention(c) (UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Void>!) -> Void)!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyCObject_AsVoidPtr(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<Void>!
@discardableResult
func PyCObject_GetDesc(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<Void>!
@discardableResult
func PyCObject_Import(_ module_name: UnsafeMutablePointer<Int8>!, _ cobject_name: UnsafeMutablePointer<Int8>!) -> UnsafeMutablePointer<Void>!
@discardableResult
func PyCObject_SetVoidPtr(_ self: UnsafeMutablePointer<PyObject>!, _ cobj: UnsafeMutablePointer<Void>!) -> Int32
struct PyCObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var cobject: UnsafeMutablePointer<Void>!
  var desc: UnsafeMutablePointer<Void>!
  var destructor: (@convention(c) (UnsafeMutablePointer<Void>!) -> Void)!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, cobject cobject: UnsafeMutablePointer<Void>!, desc desc: UnsafeMutablePointer<Void>!, destructor destructor: (@convention(c) (UnsafeMutablePointer<Void>!) -> Void)!)
}
