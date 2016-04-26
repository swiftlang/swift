
@discardableResult
func PyEval_CallObjectWithKeywords(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
func PyEval_SetProfile(_ _: Py_tracefunc!, _ _: UnsafeMutablePointer<PyObject>!)
func PyEval_SetTrace(_ _: Py_tracefunc!, _ _: UnsafeMutablePointer<PyObject>!)
@discardableResult
func PyEval_GetBuiltins() -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyEval_GetGlobals() -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyEval_GetLocals() -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyEval_GetFrame() -> UnsafeMutablePointer<_frame>!
@discardableResult
func PyEval_GetRestricted() -> Int32
@discardableResult
func PyEval_MergeCompilerFlags(_ cf: UnsafeMutablePointer<PyCompilerFlags>!) -> Int32
@discardableResult
func Py_FlushLine() -> Int32
@discardableResult
func Py_AddPendingCall(_ func: (@convention(c) (UnsafeMutablePointer<Void>!) -> Int32)!, _ arg: UnsafeMutablePointer<Void>!) -> Int32
@discardableResult
func Py_MakePendingCalls() -> Int32
func Py_SetRecursionLimit(_ _: Int32)
@discardableResult
func Py_GetRecursionLimit() -> Int32
@discardableResult
func _Py_CheckRecursiveCall(_ where: UnsafeMutablePointer<Int8>!) -> Int32
var _Py_CheckRecursionLimit: Int32
@discardableResult
func PyEval_GetFuncName(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafePointer<Int8>!
@discardableResult
func PyEval_GetFuncDesc(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafePointer<Int8>!
@discardableResult
func PyEval_GetCallStats(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyEval_EvalFrame(_ _: UnsafeMutablePointer<_frame>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyEval_EvalFrameEx(_ f: UnsafeMutablePointer<_frame>!, _ exc: Int32) -> UnsafeMutablePointer<PyObject>!
var _Py_Ticker: Int32
var _Py_CheckInterval: Int32
@discardableResult
func PyEval_SaveThread() -> UnsafeMutablePointer<PyThreadState>!
func PyEval_RestoreThread(_ _: UnsafeMutablePointer<PyThreadState>!)
@discardableResult
func PyEval_ThreadsInitialized() -> Int32
func PyEval_InitThreads()
func PyEval_AcquireLock()
func PyEval_ReleaseLock()
func PyEval_AcquireThread(_ tstate: UnsafeMutablePointer<PyThreadState>!)
func PyEval_ReleaseThread(_ tstate: UnsafeMutablePointer<PyThreadState>!)
func PyEval_ReInitThreads()
@discardableResult
func _PyEval_SliceIndex(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<Py_ssize_t>!) -> Int32
