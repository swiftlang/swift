
typealias getter = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<PyObject>!
typealias setter = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<Void>!) -> Int32
struct PyGetSetDef {
  var name: UnsafeMutablePointer<Int8>!
  var get: getter!
  var set: setter!
  var doc: UnsafeMutablePointer<Int8>!
  var closure: UnsafeMutablePointer<Void>!
  init()
  init(name name: UnsafeMutablePointer<Int8>!, get get: getter!, set set: setter!, doc doc: UnsafeMutablePointer<Int8>!, closure closure: UnsafeMutablePointer<Void>!)
}
typealias wrapperfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<PyObject>!
typealias wrapperfunc_kwds = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<Void>!, UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
struct wrapperbase {
  var name: UnsafeMutablePointer<Int8>!
  var offset: Int32
  var function: UnsafeMutablePointer<Void>!
  var wrapper: wrapperfunc!
  var doc: UnsafeMutablePointer<Int8>!
  var flags: Int32
  var name_strobj: UnsafeMutablePointer<PyObject>!
  init()
  init(name name: UnsafeMutablePointer<Int8>!, offset offset: Int32, function function: UnsafeMutablePointer<Void>!, wrapper wrapper: wrapperfunc!, doc doc: UnsafeMutablePointer<Int8>!, flags flags: Int32, name_strobj name_strobj: UnsafeMutablePointer<PyObject>!)
}
var PyWrapperFlag_KEYWORDS: Int32 { get }
struct PyDescrObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var d_type: UnsafeMutablePointer<PyTypeObject>!
  var d_name: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, d_type d_type: UnsafeMutablePointer<PyTypeObject>!, d_name d_name: UnsafeMutablePointer<PyObject>!)
}
struct PyMethodDescrObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var d_type: UnsafeMutablePointer<PyTypeObject>!
  var d_name: UnsafeMutablePointer<PyObject>!
  var d_method: UnsafeMutablePointer<PyMethodDef>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, d_type d_type: UnsafeMutablePointer<PyTypeObject>!, d_name d_name: UnsafeMutablePointer<PyObject>!, d_method d_method: UnsafeMutablePointer<PyMethodDef>!)
}
struct PyMemberDescrObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var d_type: UnsafeMutablePointer<PyTypeObject>!
  var d_name: UnsafeMutablePointer<PyObject>!
  var d_member: UnsafeMutablePointer<PyMemberDef>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, d_type d_type: UnsafeMutablePointer<PyTypeObject>!, d_name d_name: UnsafeMutablePointer<PyObject>!, d_member d_member: UnsafeMutablePointer<PyMemberDef>!)
}
struct PyGetSetDescrObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var d_type: UnsafeMutablePointer<PyTypeObject>!
  var d_name: UnsafeMutablePointer<PyObject>!
  var d_getset: UnsafeMutablePointer<PyGetSetDef>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, d_type d_type: UnsafeMutablePointer<PyTypeObject>!, d_name d_name: UnsafeMutablePointer<PyObject>!, d_getset d_getset: UnsafeMutablePointer<PyGetSetDef>!)
}
struct PyWrapperDescrObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var d_type: UnsafeMutablePointer<PyTypeObject>!
  var d_name: UnsafeMutablePointer<PyObject>!
  var d_base: UnsafeMutablePointer<wrapperbase>!
  var d_wrapped: UnsafeMutablePointer<Void>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, d_type d_type: UnsafeMutablePointer<PyTypeObject>!, d_name d_name: UnsafeMutablePointer<PyObject>!, d_base d_base: UnsafeMutablePointer<wrapperbase>!, d_wrapped d_wrapped: UnsafeMutablePointer<Void>!)
}
var PyWrapperDescr_Type: PyTypeObject
var PyDictProxy_Type: PyTypeObject
var PyGetSetDescr_Type: PyTypeObject
var PyMemberDescr_Type: PyTypeObject
@discardableResult
func PyDescr_NewMethod(_ _: UnsafeMutablePointer<PyTypeObject>!, _ _: UnsafeMutablePointer<PyMethodDef>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDescr_NewClassMethod(_ _: UnsafeMutablePointer<PyTypeObject>!, _ _: UnsafeMutablePointer<PyMethodDef>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDescr_NewMember(_ _: UnsafeMutablePointer<PyTypeObject>!, _ _: UnsafeMutablePointer<PyMemberDef>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDescr_NewGetSet(_ _: UnsafeMutablePointer<PyTypeObject>!, _ _: UnsafeMutablePointer<PyGetSetDef>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDescr_NewWrapper(_ _: UnsafeMutablePointer<PyTypeObject>!, _ _: UnsafeMutablePointer<wrapperbase>!, _ _: UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDictProxy_New(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyWrapper_New(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
var PyProperty_Type: PyTypeObject
