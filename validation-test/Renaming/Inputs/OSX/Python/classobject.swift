
struct PyClassObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var cl_bases: UnsafeMutablePointer<PyObject>!
  var cl_dict: UnsafeMutablePointer<PyObject>!
  var cl_name: UnsafeMutablePointer<PyObject>!
  var cl_getattr: UnsafeMutablePointer<PyObject>!
  var cl_setattr: UnsafeMutablePointer<PyObject>!
  var cl_delattr: UnsafeMutablePointer<PyObject>!
  var cl_weakreflist: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, cl_bases cl_bases: UnsafeMutablePointer<PyObject>!, cl_dict cl_dict: UnsafeMutablePointer<PyObject>!, cl_name cl_name: UnsafeMutablePointer<PyObject>!, cl_getattr cl_getattr: UnsafeMutablePointer<PyObject>!, cl_setattr cl_setattr: UnsafeMutablePointer<PyObject>!, cl_delattr cl_delattr: UnsafeMutablePointer<PyObject>!, cl_weakreflist cl_weakreflist: UnsafeMutablePointer<PyObject>!)
}
struct PyInstanceObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var in_class: UnsafeMutablePointer<PyClassObject>!
  var in_dict: UnsafeMutablePointer<PyObject>!
  var in_weakreflist: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, in_class in_class: UnsafeMutablePointer<PyClassObject>!, in_dict in_dict: UnsafeMutablePointer<PyObject>!, in_weakreflist in_weakreflist: UnsafeMutablePointer<PyObject>!)
}
struct PyMethodObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var im_func: UnsafeMutablePointer<PyObject>!
  var im_self: UnsafeMutablePointer<PyObject>!
  var im_class: UnsafeMutablePointer<PyObject>!
  var im_weakreflist: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, im_func im_func: UnsafeMutablePointer<PyObject>!, im_self im_self: UnsafeMutablePointer<PyObject>!, im_class im_class: UnsafeMutablePointer<PyObject>!, im_weakreflist im_weakreflist: UnsafeMutablePointer<PyObject>!)
}
var PyClass_Type: PyTypeObject
var PyInstance_Type: PyTypeObject
var PyMethod_Type: PyTypeObject
@discardableResult
func PyClass_New(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyInstance_New(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyInstance_NewRaw(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyMethod_New(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyMethod_Function(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyMethod_Self(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyMethod_Class(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyInstance_Lookup(_ pinst: UnsafeMutablePointer<PyObject>!, _ name: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyClass_IsSubclass(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyMethod_ClearFreeList() -> Int32
