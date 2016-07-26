
@discardableResult
func PyEval_EvalCode(_ _: UnsafeMutablePointer<PyCodeObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyEval_EvalCodeEx(_ co: UnsafeMutablePointer<PyCodeObject>!, _ globals: UnsafeMutablePointer<PyObject>!, _ locals: UnsafeMutablePointer<PyObject>!, _ args: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ argc: Int32, _ kwds: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ kwdc: Int32, _ defs: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ defc: Int32, _ closure: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyEval_CallTracing(_ func: UnsafeMutablePointer<PyObject>!, _ args: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
