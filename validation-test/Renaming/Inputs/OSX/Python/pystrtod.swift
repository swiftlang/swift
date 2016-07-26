
@discardableResult
func PyOS_ascii_strtod(_ str: UnsafePointer<Int8>!, _ ptr: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!) -> Double
@discardableResult
func PyOS_ascii_atof(_ str: UnsafePointer<Int8>!) -> Double
@discardableResult
func PyOS_ascii_formatd(_ buffer: UnsafeMutablePointer<Int8>!, _ buf_len: Int, _ format: UnsafePointer<Int8>!, _ d: Double) -> UnsafeMutablePointer<Int8>!
@discardableResult
func PyOS_string_to_double(_ str: UnsafePointer<Int8>!, _ endptr: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!, _ overflow_exception: UnsafeMutablePointer<PyObject>!) -> Double
@discardableResult
func PyOS_double_to_string(_ val: Double, _ format_code: Int8, _ precision: Int32, _ flags: Int32, _ type: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Int8>!
@discardableResult
func _Py_parse_inf_or_nan(_ p: UnsafePointer<Int8>!, _ endptr: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!) -> Double
var Py_DTSF_SIGN: Int32 { get }
var Py_DTSF_ADD_DOT_0: Int32 { get }
var Py_DTSF_ALT: Int32 { get }
var Py_DTST_FINITE: Int32 { get }
var Py_DTST_INFINITE: Int32 { get }
var Py_DTST_NAN: Int32 { get }
