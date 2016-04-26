
var PyCFunction_Type: PyTypeObject
typealias PyCFunction = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
typealias PyCFunctionWithKeywords = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
typealias PyNoArgsFunction = @convention(c) (UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyCFunction_GetFunction(_ _: UnsafeMutablePointer<PyObject>!) -> PyCFunction!
@discardableResult
func PyCFunction_GetSelf(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyCFunction_GetFlags(_ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyCFunction_Call(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
struct PyMethodDef {
  var ml_name: UnsafePointer<Int8>!
  var ml_meth: PyCFunction!
  var ml_flags: Int32
  var ml_doc: UnsafePointer<Int8>!
  init()
  init(ml_name ml_name: UnsafePointer<Int8>!, ml_meth ml_meth: PyCFunction!, ml_flags ml_flags: Int32, ml_doc ml_doc: UnsafePointer<Int8>!)
}
@discardableResult
func Py_FindMethod(_ _: UnsafeMutablePointer<PyMethodDef>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyCFunction_NewEx(_ _: UnsafeMutablePointer<PyMethodDef>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
var METH_OLDARGS: Int32 { get }
var METH_VARARGS: Int32 { get }
var METH_KEYWORDS: Int32 { get }
var METH_NOARGS: Int32 { get }
var METH_O: Int32 { get }
var METH_CLASS: Int32 { get }
var METH_STATIC: Int32 { get }
var METH_COEXIST: Int32 { get }
struct PyMethodChain {
  var methods: UnsafeMutablePointer<PyMethodDef>!
  var link: UnsafeMutablePointer<PyMethodChain>!
  init()
  init(methods methods: UnsafeMutablePointer<PyMethodDef>!, link link: UnsafeMutablePointer<PyMethodChain>!)
}
@discardableResult
func Py_FindMethodInChain(_ _: UnsafeMutablePointer<PyMethodChain>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
struct PyCFunctionObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var m_ml: UnsafeMutablePointer<PyMethodDef>!
  var m_self: UnsafeMutablePointer<PyObject>!
  var m_module: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, m_ml m_ml: UnsafeMutablePointer<PyMethodDef>!, m_self m_self: UnsafeMutablePointer<PyObject>!, m_module m_module: UnsafeMutablePointer<PyObject>!)
}
@discardableResult
func PyCFunction_ClearFreeList() -> Int32
