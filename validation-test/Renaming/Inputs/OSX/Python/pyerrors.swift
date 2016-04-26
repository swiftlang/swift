
struct PyBaseExceptionObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var dict: UnsafeMutablePointer<PyObject>!
  var args: UnsafeMutablePointer<PyObject>!
  var message: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, dict dict: UnsafeMutablePointer<PyObject>!, args args: UnsafeMutablePointer<PyObject>!, message message: UnsafeMutablePointer<PyObject>!)
}
struct PySyntaxErrorObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var dict: UnsafeMutablePointer<PyObject>!
  var args: UnsafeMutablePointer<PyObject>!
  var message: UnsafeMutablePointer<PyObject>!
  var msg: UnsafeMutablePointer<PyObject>!
  var filename: UnsafeMutablePointer<PyObject>!
  var lineno: UnsafeMutablePointer<PyObject>!
  var offset: UnsafeMutablePointer<PyObject>!
  var text: UnsafeMutablePointer<PyObject>!
  var print_file_and_line: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, dict dict: UnsafeMutablePointer<PyObject>!, args args: UnsafeMutablePointer<PyObject>!, message message: UnsafeMutablePointer<PyObject>!, msg msg: UnsafeMutablePointer<PyObject>!, filename filename: UnsafeMutablePointer<PyObject>!, lineno lineno: UnsafeMutablePointer<PyObject>!, offset offset: UnsafeMutablePointer<PyObject>!, text text: UnsafeMutablePointer<PyObject>!, print_file_and_line print_file_and_line: UnsafeMutablePointer<PyObject>!)
}
struct PyUnicodeErrorObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var dict: UnsafeMutablePointer<PyObject>!
  var args: UnsafeMutablePointer<PyObject>!
  var message: UnsafeMutablePointer<PyObject>!
  var encoding: UnsafeMutablePointer<PyObject>!
  var object: UnsafeMutablePointer<PyObject>!
  var start: Py_ssize_t
  var end: Py_ssize_t
  var reason: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, dict dict: UnsafeMutablePointer<PyObject>!, args args: UnsafeMutablePointer<PyObject>!, message message: UnsafeMutablePointer<PyObject>!, encoding encoding: UnsafeMutablePointer<PyObject>!, object object: UnsafeMutablePointer<PyObject>!, start start: Py_ssize_t, end end: Py_ssize_t, reason reason: UnsafeMutablePointer<PyObject>!)
}
struct PySystemExitObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var dict: UnsafeMutablePointer<PyObject>!
  var args: UnsafeMutablePointer<PyObject>!
  var message: UnsafeMutablePointer<PyObject>!
  var code: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, dict dict: UnsafeMutablePointer<PyObject>!, args args: UnsafeMutablePointer<PyObject>!, message message: UnsafeMutablePointer<PyObject>!, code code: UnsafeMutablePointer<PyObject>!)
}
struct PyEnvironmentErrorObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var dict: UnsafeMutablePointer<PyObject>!
  var args: UnsafeMutablePointer<PyObject>!
  var message: UnsafeMutablePointer<PyObject>!
  var myerrno: UnsafeMutablePointer<PyObject>!
  var strerror: UnsafeMutablePointer<PyObject>!
  var filename: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, dict dict: UnsafeMutablePointer<PyObject>!, args args: UnsafeMutablePointer<PyObject>!, message message: UnsafeMutablePointer<PyObject>!, myerrno myerrno: UnsafeMutablePointer<PyObject>!, strerror strerror: UnsafeMutablePointer<PyObject>!, filename filename: UnsafeMutablePointer<PyObject>!)
}
func PyErr_SetNone(_ _: UnsafeMutablePointer<PyObject>!)
func PyErr_SetObject(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!)
func PyErr_SetString(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!)
@discardableResult
func PyErr_Occurred() -> UnsafeMutablePointer<PyObject>!
func PyErr_Clear()
func PyErr_Fetch(_ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!)
func PyErr_Restore(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!)
@discardableResult
func PyErr_GivenExceptionMatches(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyErr_ExceptionMatches(_ _: UnsafeMutablePointer<PyObject>!) -> Int32
func PyErr_NormalizeException(_ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!)
func _PyErr_ReplaceException(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!)
var PyExc_BaseException: UnsafeMutablePointer<PyObject>!
var PyExc_Exception: UnsafeMutablePointer<PyObject>!
var PyExc_StopIteration: UnsafeMutablePointer<PyObject>!
var PyExc_GeneratorExit: UnsafeMutablePointer<PyObject>!
var PyExc_StandardError: UnsafeMutablePointer<PyObject>!
var PyExc_ArithmeticError: UnsafeMutablePointer<PyObject>!
var PyExc_LookupError: UnsafeMutablePointer<PyObject>!
var PyExc_AssertionError: UnsafeMutablePointer<PyObject>!
var PyExc_AttributeError: UnsafeMutablePointer<PyObject>!
var PyExc_EOFError: UnsafeMutablePointer<PyObject>!
var PyExc_FloatingPointError: UnsafeMutablePointer<PyObject>!
var PyExc_EnvironmentError: UnsafeMutablePointer<PyObject>!
var PyExc_IOError: UnsafeMutablePointer<PyObject>!
var PyExc_OSError: UnsafeMutablePointer<PyObject>!
var PyExc_ImportError: UnsafeMutablePointer<PyObject>!
var PyExc_IndexError: UnsafeMutablePointer<PyObject>!
var PyExc_KeyError: UnsafeMutablePointer<PyObject>!
var PyExc_KeyboardInterrupt: UnsafeMutablePointer<PyObject>!
var PyExc_MemoryError: UnsafeMutablePointer<PyObject>!
var PyExc_NameError: UnsafeMutablePointer<PyObject>!
var PyExc_OverflowError: UnsafeMutablePointer<PyObject>!
var PyExc_RuntimeError: UnsafeMutablePointer<PyObject>!
var PyExc_NotImplementedError: UnsafeMutablePointer<PyObject>!
var PyExc_SyntaxError: UnsafeMutablePointer<PyObject>!
var PyExc_IndentationError: UnsafeMutablePointer<PyObject>!
var PyExc_TabError: UnsafeMutablePointer<PyObject>!
var PyExc_ReferenceError: UnsafeMutablePointer<PyObject>!
var PyExc_SystemError: UnsafeMutablePointer<PyObject>!
var PyExc_SystemExit: UnsafeMutablePointer<PyObject>!
var PyExc_TypeError: UnsafeMutablePointer<PyObject>!
var PyExc_UnboundLocalError: UnsafeMutablePointer<PyObject>!
var PyExc_UnicodeError: UnsafeMutablePointer<PyObject>!
var PyExc_UnicodeEncodeError: UnsafeMutablePointer<PyObject>!
var PyExc_UnicodeDecodeError: UnsafeMutablePointer<PyObject>!
var PyExc_UnicodeTranslateError: UnsafeMutablePointer<PyObject>!
var PyExc_ValueError: UnsafeMutablePointer<PyObject>!
var PyExc_ZeroDivisionError: UnsafeMutablePointer<PyObject>!
var PyExc_BufferError: UnsafeMutablePointer<PyObject>!
var PyExc_MemoryErrorInst: UnsafeMutablePointer<PyObject>!
var PyExc_RecursionErrorInst: UnsafeMutablePointer<PyObject>!
var PyExc_Warning: UnsafeMutablePointer<PyObject>!
var PyExc_UserWarning: UnsafeMutablePointer<PyObject>!
var PyExc_DeprecationWarning: UnsafeMutablePointer<PyObject>!
var PyExc_PendingDeprecationWarning: UnsafeMutablePointer<PyObject>!
var PyExc_SyntaxWarning: UnsafeMutablePointer<PyObject>!
var PyExc_RuntimeWarning: UnsafeMutablePointer<PyObject>!
var PyExc_FutureWarning: UnsafeMutablePointer<PyObject>!
var PyExc_ImportWarning: UnsafeMutablePointer<PyObject>!
var PyExc_UnicodeWarning: UnsafeMutablePointer<PyObject>!
var PyExc_BytesWarning: UnsafeMutablePointer<PyObject>!
@discardableResult
func PyErr_BadArgument() -> Int32
@discardableResult
func PyErr_NoMemory() -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyErr_SetFromErrno(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyErr_SetFromErrnoWithFilenameObject(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyErr_SetFromErrnoWithFilename(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
func PyErr_BadInternalCall()
func _PyErr_BadInternalCall(_ filename: UnsafeMutablePointer<Int8>!, _ lineno: Int32)
@discardableResult
func PyErr_NewException(_ name: UnsafeMutablePointer<Int8>!, _ base: UnsafeMutablePointer<PyObject>!, _ dict: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyErr_NewExceptionWithDoc(_ name: UnsafeMutablePointer<Int8>!, _ doc: UnsafeMutablePointer<Int8>!, _ base: UnsafeMutablePointer<PyObject>!, _ dict: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
func PyErr_WriteUnraisable(_ _: UnsafeMutablePointer<PyObject>!)
@discardableResult
func PyErr_CheckSignals() -> Int32
func PyErr_SetInterrupt()
@discardableResult
func PySignal_SetWakeupFd(_ fd: Int32) -> Int32
func PyErr_SyntaxLocation(_ _: UnsafePointer<Int8>!, _ _: Int32)
@discardableResult
func PyErr_ProgramText(_ _: UnsafePointer<Int8>!, _ _: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeDecodeError_Create(_ _: UnsafePointer<Int8>!, _ _: UnsafePointer<Int8>!, _ _: Py_ssize_t, _ _: Py_ssize_t, _ _: Py_ssize_t, _ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeEncodeError_Create(_ _: UnsafePointer<Int8>!, _ _: UnsafePointer<Py_UNICODE>!, _ _: Py_ssize_t, _ _: Py_ssize_t, _ _: Py_ssize_t, _ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeTranslateError_Create(_ _: UnsafePointer<Py_UNICODE>!, _ _: Py_ssize_t, _ _: Py_ssize_t, _ _: Py_ssize_t, _ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeEncodeError_GetEncoding(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeDecodeError_GetEncoding(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeEncodeError_GetObject(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeDecodeError_GetObject(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeTranslateError_GetObject(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeEncodeError_GetStart(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<Py_ssize_t>!) -> Int32
@discardableResult
func PyUnicodeDecodeError_GetStart(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<Py_ssize_t>!) -> Int32
@discardableResult
func PyUnicodeTranslateError_GetStart(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<Py_ssize_t>!) -> Int32
@discardableResult
func PyUnicodeEncodeError_SetStart(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t) -> Int32
@discardableResult
func PyUnicodeDecodeError_SetStart(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t) -> Int32
@discardableResult
func PyUnicodeTranslateError_SetStart(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t) -> Int32
@discardableResult
func PyUnicodeEncodeError_GetEnd(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<Py_ssize_t>!) -> Int32
@discardableResult
func PyUnicodeDecodeError_GetEnd(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<Py_ssize_t>!) -> Int32
@discardableResult
func PyUnicodeTranslateError_GetEnd(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<Py_ssize_t>!) -> Int32
@discardableResult
func PyUnicodeEncodeError_SetEnd(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t) -> Int32
@discardableResult
func PyUnicodeDecodeError_SetEnd(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t) -> Int32
@discardableResult
func PyUnicodeTranslateError_SetEnd(_ _: UnsafeMutablePointer<PyObject>!, _ _: Py_ssize_t) -> Int32
@discardableResult
func PyUnicodeEncodeError_GetReason(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeDecodeError_GetReason(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeTranslateError_GetReason(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyUnicodeEncodeError_SetReason(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!) -> Int32
@discardableResult
func PyUnicodeDecodeError_SetReason(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!) -> Int32
@discardableResult
func PyUnicodeTranslateError_SetReason(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!) -> Int32
@discardableResult
func PyOS_vsnprintf(_ str: UnsafeMutablePointer<Int8>!, _ size: Int, _ format: UnsafePointer<Int8>!, _ va: CVaListPointer) -> Int32
