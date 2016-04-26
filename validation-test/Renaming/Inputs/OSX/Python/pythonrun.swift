
var PyCF_MASK_OBSOLETE: Int32 { get }
var PyCF_SOURCE_IS_UTF8: Int32 { get }
var PyCF_DONT_IMPLY_DEDENT: Int32 { get }
var PyCF_ONLY_AST: Int32 { get }
struct PyCompilerFlags {
  var cf_flags: Int32
  init()
  init(cf_flags cf_flags: Int32)
}
func Py_SetProgramName(_ _: UnsafeMutablePointer<Int8>!)
@discardableResult
func Py_GetProgramName() -> UnsafeMutablePointer<Int8>!
func Py_SetPythonHome(_ _: UnsafeMutablePointer<Int8>!)
@discardableResult
func Py_GetPythonHome() -> UnsafeMutablePointer<Int8>!
func Py_Initialize()
func Py_InitializeEx(_ _: Int32)
func Py_Finalize()
@discardableResult
func Py_IsInitialized() -> Int32
@discardableResult
func Py_NewInterpreter() -> UnsafeMutablePointer<PyThreadState>!
func Py_EndInterpreter(_ _: UnsafeMutablePointer<PyThreadState>!)
@discardableResult
func PyRun_AnyFileFlags(_ _: UnsafeMutablePointer<FILE>!, _ _: UnsafePointer<Int8>!, _ _: UnsafeMutablePointer<PyCompilerFlags>!) -> Int32
@discardableResult
func PyRun_AnyFileExFlags(_ _: UnsafeMutablePointer<FILE>!, _ _: UnsafePointer<Int8>!, _ _: Int32, _ _: UnsafeMutablePointer<PyCompilerFlags>!) -> Int32
@discardableResult
func PyRun_SimpleStringFlags(_ _: UnsafePointer<Int8>!, _ _: UnsafeMutablePointer<PyCompilerFlags>!) -> Int32
@discardableResult
func PyRun_SimpleFileExFlags(_ _: UnsafeMutablePointer<FILE>!, _ _: UnsafePointer<Int8>!, _ _: Int32, _ _: UnsafeMutablePointer<PyCompilerFlags>!) -> Int32
@discardableResult
func PyRun_InteractiveOneFlags(_ _: UnsafeMutablePointer<FILE>!, _ _: UnsafePointer<Int8>!, _ _: UnsafeMutablePointer<PyCompilerFlags>!) -> Int32
@discardableResult
func PyRun_InteractiveLoopFlags(_ _: UnsafeMutablePointer<FILE>!, _ _: UnsafePointer<Int8>!, _ _: UnsafeMutablePointer<PyCompilerFlags>!) -> Int32
@discardableResult
func PyParser_ASTFromString(_ _: UnsafePointer<Int8>!, _ _: UnsafePointer<Int8>!, _ _: Int32, _ flags: UnsafeMutablePointer<PyCompilerFlags>!, _ _: OpaquePointer!) -> OpaquePointer!
@discardableResult
func PyParser_ASTFromFile(_ _: UnsafeMutablePointer<FILE>!, _ _: UnsafePointer<Int8>!, _ _: Int32, _ _: UnsafeMutablePointer<Int8>!, _ _: UnsafeMutablePointer<Int8>!, _ _: UnsafeMutablePointer<PyCompilerFlags>!, _ _: UnsafeMutablePointer<Int32>!, _ _: OpaquePointer!) -> OpaquePointer!
@discardableResult
func PyParser_SimpleParseStringFlags(_ _: UnsafePointer<Int8>!, _ _: Int32, _ _: Int32) -> UnsafeMutablePointer<_node>!
@discardableResult
func PyParser_SimpleParseFileFlags(_ _: UnsafeMutablePointer<FILE>!, _ _: UnsafePointer<Int8>!, _ _: Int32, _ _: Int32) -> UnsafeMutablePointer<_node>!
@discardableResult
func PyRun_StringFlags(_ _: UnsafePointer<Int8>!, _ _: Int32, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyCompilerFlags>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyRun_FileExFlags(_ _: UnsafeMutablePointer<FILE>!, _ _: UnsafePointer<Int8>!, _ _: Int32, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: Int32, _ _: UnsafeMutablePointer<PyCompilerFlags>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func Py_CompileStringFlags(_ _: UnsafePointer<Int8>!, _ _: UnsafePointer<Int8>!, _ _: Int32, _ _: UnsafeMutablePointer<PyCompilerFlags>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func Py_SymtableString(_ _: UnsafePointer<Int8>!, _ _: UnsafePointer<Int8>!, _ _: Int32) -> OpaquePointer!
func PyErr_Print()
func PyErr_PrintEx(_ _: Int32)
func PyErr_Display(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!)
@discardableResult
func Py_AtExit(_ func: (@convention(c) () -> Void)!) -> Int32
func Py_Exit(_ _: Int32)
@discardableResult
func Py_FdIsInteractive(_ _: UnsafeMutablePointer<FILE>!, _ _: UnsafePointer<Int8>!) -> Int32
@discardableResult
func Py_Main(_ argc: Int32, _ argv: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!) -> Int32
@discardableResult
func Py_GetProgramFullPath() -> UnsafeMutablePointer<Int8>!
@discardableResult
func Py_GetPrefix() -> UnsafeMutablePointer<Int8>!
@discardableResult
func Py_GetExecPrefix() -> UnsafeMutablePointer<Int8>!
@discardableResult
func Py_GetPath() -> UnsafeMutablePointer<Int8>!
@discardableResult
func Py_GetVersion() -> UnsafePointer<Int8>!
@discardableResult
func Py_GetPlatform() -> UnsafePointer<Int8>!
@discardableResult
func Py_GetCopyright() -> UnsafePointer<Int8>!
@discardableResult
func Py_GetCompiler() -> UnsafePointer<Int8>!
@discardableResult
func Py_GetBuildInfo() -> UnsafePointer<Int8>!
@discardableResult
func _Py_svnversion() -> UnsafePointer<Int8>!
@discardableResult
func Py_SubversionRevision() -> UnsafePointer<Int8>!
@discardableResult
func Py_SubversionShortBranch() -> UnsafePointer<Int8>!
@discardableResult
func _Py_hgidentifier() -> UnsafePointer<Int8>!
@discardableResult
func _Py_hgversion() -> UnsafePointer<Int8>!
@discardableResult
func _PyBuiltin_Init() -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PySys_Init() -> UnsafeMutablePointer<PyObject>!
func _PyImport_Init()
func _PyExc_Init()
func _PyImportHooks_Init()
@discardableResult
func _PyFrame_Init() -> Int32
@discardableResult
func _PyInt_Init() -> Int32
@discardableResult
func _PyLong_Init() -> Int32
func _PyFloat_Init()
@discardableResult
func PyByteArray_Init() -> Int32
func _PyRandom_Init()
func _PyExc_Fini()
func _PyImport_Fini()
func PyMethod_Fini()
func PyFrame_Fini()
func PyCFunction_Fini()
func PyDict_Fini()
func PyTuple_Fini()
func PyList_Fini()
func PySet_Fini()
func PyString_Fini()
func PyInt_Fini()
func PyFloat_Fini()
func PyOS_FiniInterrupts()
func PyByteArray_Fini()
func _PyRandom_Fini()
@discardableResult
func PyOS_Readline(_ _: UnsafeMutablePointer<FILE>!, _ _: UnsafeMutablePointer<FILE>!, _ _: UnsafeMutablePointer<Int8>!) -> UnsafeMutablePointer<Int8>!
var PyOS_InputHook: (@convention(c) () -> Int32)!
var PyOS_ReadlineFunctionPointer: (@convention(c) (UnsafeMutablePointer<FILE>!, UnsafeMutablePointer<FILE>!, UnsafeMutablePointer<Int8>!) -> UnsafeMutablePointer<Int8>!)!
var _PyOS_ReadlineTState: UnsafeMutablePointer<PyThreadState>!
var PYOS_STACK_MARGIN: Int32 { get }
typealias PyOS_sighandler_t = @convention(c) (Int32) -> Void
@discardableResult
func PyOS_getsig(_ _: Int32) -> PyOS_sighandler_t!
@discardableResult
func PyOS_setsig(_ _: Int32, _ _: PyOS_sighandler_t!) -> PyOS_sighandler_t!
@discardableResult
func _PyOS_URandom(_ buffer: UnsafeMutablePointer<Void>!, _ size: Py_ssize_t) -> Int32
