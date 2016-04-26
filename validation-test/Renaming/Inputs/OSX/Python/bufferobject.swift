
var PyBuffer_Type: PyTypeObject
var Py_END_OF_BUFFER: Int32 { get }
@discardableResult
func PyBuffer_FromObject(_ base: UnsafeMutablePointer<PyObject>!, _ offset: Py_ssize_t, _ size: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyBuffer_FromReadWriteObject(_ base: UnsafeMutablePointer<PyObject>!, _ offset: Py_ssize_t, _ size: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyBuffer_FromMemory(_ ptr: UnsafeMutablePointer<Void>!, _ size: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyBuffer_FromReadWriteMemory(_ ptr: UnsafeMutablePointer<Void>!, _ size: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyBuffer_New(_ size: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
