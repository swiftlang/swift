
struct PyFunctionObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var func_code: UnsafeMutablePointer<PyObject>!
  var func_globals: UnsafeMutablePointer<PyObject>!
  var func_defaults: UnsafeMutablePointer<PyObject>!
  var func_closure: UnsafeMutablePointer<PyObject>!
  var func_doc: UnsafeMutablePointer<PyObject>!
  var func_name: UnsafeMutablePointer<PyObject>!
  var func_dict: UnsafeMutablePointer<PyObject>!
  var func_weakreflist: UnsafeMutablePointer<PyObject>!
  var func_module: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, func_code func_code: UnsafeMutablePointer<PyObject>!, func_globals func_globals: UnsafeMutablePointer<PyObject>!, func_defaults func_defaults: UnsafeMutablePointer<PyObject>!, func_closure func_closure: UnsafeMutablePointer<PyObject>!, func_doc func_doc: UnsafeMutablePointer<PyObject>!, func_name func_name: UnsafeMutablePointer<PyObject>!, func_dict func_dict: UnsafeMutablePointer<PyObject>!, func_weakreflist func_weakreflist: UnsafeMutablePointer<PyObject>!, func_module func_module: UnsafeMutablePointer<PyObject>!)
}
var PyFunction_Type: PyTypeObject
@discardableResult
func PyFunction_New(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyFunction_GetCode(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyFunction_GetGlobals(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyFunction_GetModule(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyFunction_GetDefaults(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyFunction_SetDefaults(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyFunction_GetClosure(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyFunction_SetClosure(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
var PyClassMethod_Type: PyTypeObject
var PyStaticMethod_Type: PyTypeObject
@discardableResult
func PyClassMethod_New(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyStaticMethod_New(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
