
@discardableResult
func _Py_VaBuildValue_SizeT(_ _: UnsafePointer<Int8>!, _ _: CVaListPointer) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyArg_NoKeywords(_ funcname: UnsafePointer<Int8>!, _ kw: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyArg_VaParse(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!, _ _: CVaListPointer) -> Int32
@discardableResult
func PyArg_VaParseTupleAndKeywords(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!, _ _: CVaListPointer) -> Int32
@discardableResult
func Py_VaBuildValue(_ _: UnsafePointer<Int8>!, _ _: CVaListPointer) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyModule_AddObject(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyModule_AddIntConstant(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!, _ _: Int) -> Int32
@discardableResult
func PyModule_AddStringConstant(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!, _ _: UnsafePointer<Int8>!) -> Int32
var PYTHON_API_VERSION: Int32 { get }
var PYTHON_API_STRING: String { get }
@discardableResult
func Py_InitModule4_64(_ name: UnsafePointer<Int8>!, _ methods: UnsafeMutablePointer<PyMethodDef>!, _ doc: UnsafePointer<Int8>!, _ self: UnsafeMutablePointer<PyObject>!, _ apiver: Int32) -> UnsafeMutablePointer<PyObject>!
var _Py_PackageContext: UnsafeMutablePointer<Int8>!
