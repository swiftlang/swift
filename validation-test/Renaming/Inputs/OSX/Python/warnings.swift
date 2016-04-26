
func _PyWarnings_Init()
@discardableResult
func PyErr_WarnEx(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!, _ _: Py_ssize_t) -> Int32
@discardableResult
func PyErr_WarnExplicit(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!, _ _: UnsafePointer<Int8>!, _ _: Int32, _ _: UnsafePointer<Int8>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
