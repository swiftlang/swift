
struct PyCodeObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var co_argcount: Int32
  var co_nlocals: Int32
  var co_stacksize: Int32
  var co_flags: Int32
  var co_code: UnsafeMutablePointer<PyObject>!
  var co_consts: UnsafeMutablePointer<PyObject>!
  var co_names: UnsafeMutablePointer<PyObject>!
  var co_varnames: UnsafeMutablePointer<PyObject>!
  var co_freevars: UnsafeMutablePointer<PyObject>!
  var co_cellvars: UnsafeMutablePointer<PyObject>!
  var co_filename: UnsafeMutablePointer<PyObject>!
  var co_name: UnsafeMutablePointer<PyObject>!
  var co_firstlineno: Int32
  var co_lnotab: UnsafeMutablePointer<PyObject>!
  var co_zombieframe: UnsafeMutablePointer<Void>!
  var co_weakreflist: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, co_argcount co_argcount: Int32, co_nlocals co_nlocals: Int32, co_stacksize co_stacksize: Int32, co_flags co_flags: Int32, co_code co_code: UnsafeMutablePointer<PyObject>!, co_consts co_consts: UnsafeMutablePointer<PyObject>!, co_names co_names: UnsafeMutablePointer<PyObject>!, co_varnames co_varnames: UnsafeMutablePointer<PyObject>!, co_freevars co_freevars: UnsafeMutablePointer<PyObject>!, co_cellvars co_cellvars: UnsafeMutablePointer<PyObject>!, co_filename co_filename: UnsafeMutablePointer<PyObject>!, co_name co_name: UnsafeMutablePointer<PyObject>!, co_firstlineno co_firstlineno: Int32, co_lnotab co_lnotab: UnsafeMutablePointer<PyObject>!, co_zombieframe co_zombieframe: UnsafeMutablePointer<Void>!, co_weakreflist co_weakreflist: UnsafeMutablePointer<PyObject>!)
}
var CO_OPTIMIZED: Int32 { get }
var CO_NEWLOCALS: Int32 { get }
var CO_VARARGS: Int32 { get }
var CO_VARKEYWORDS: Int32 { get }
var CO_NESTED: Int32 { get }
var CO_GENERATOR: Int32 { get }
var CO_NOFREE: Int32 { get }
var CO_FUTURE_DIVISION: Int32 { get }
var CO_FUTURE_ABSOLUTE_IMPORT: Int32 { get }
var CO_FUTURE_WITH_STATEMENT: Int32 { get }
var CO_FUTURE_PRINT_FUNCTION: Int32 { get }
var CO_FUTURE_UNICODE_LITERALS: Int32 { get }
var CO_MAXBLOCKS: Int32 { get }
var PyCode_Type: PyTypeObject
@discardableResult
func PyCode_New(_ _: Int32, _ _: Int32, _ _: Int32, _ _: Int32, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: Int32, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyCodeObject>!
@discardableResult
func PyCode_NewEmpty(_ filename: UnsafePointer<Int8>!, _ funcname: UnsafePointer<Int8>!, _ firstlineno: Int32) -> UnsafeMutablePointer<PyCodeObject>!
@discardableResult
func PyCode_Addr2Line(_ _: UnsafeMutablePointer<PyCodeObject>!, _ _: Int32) -> Int32
struct _addr_pair {
  var ap_lower: Int32
  var ap_upper: Int32
  init()
  init(ap_lower ap_lower: Int32, ap_upper ap_upper: Int32)
}
typealias PyAddrPair = _addr_pair
@discardableResult
func _PyCode_CheckLineNumber(_ co: UnsafeMutablePointer<PyCodeObject>!, _ lasti: Int32, _ bounds: UnsafeMutablePointer<PyAddrPair>!) -> Int32
@discardableResult
func PyCode_Optimize(_ code: UnsafeMutablePointer<PyObject>!, _ consts: UnsafeMutablePointer<PyObject>!, _ names: UnsafeMutablePointer<PyObject>!, _ lineno_obj: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
