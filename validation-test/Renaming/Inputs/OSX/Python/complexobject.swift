
struct Py_complex {
  var real: Double
  var imag: Double
  init()
  init(real real: Double, imag imag: Double)
}
@discardableResult
func _Py_c_sum(_ _: Py_complex, _ _: Py_complex) -> Py_complex
@discardableResult
func _Py_c_diff(_ _: Py_complex, _ _: Py_complex) -> Py_complex
@discardableResult
func _Py_c_neg(_ _: Py_complex) -> Py_complex
@discardableResult
func _Py_c_prod(_ _: Py_complex, _ _: Py_complex) -> Py_complex
@discardableResult
func _Py_c_quot(_ _: Py_complex, _ _: Py_complex) -> Py_complex
@discardableResult
func _Py_c_pow(_ _: Py_complex, _ _: Py_complex) -> Py_complex
@discardableResult
func _Py_c_abs(_ _: Py_complex) -> Double
struct PyComplexObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var cval: Py_complex
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, cval cval: Py_complex)
}
var PyComplex_Type: PyTypeObject
@discardableResult
func PyComplex_FromCComplex(_ _: Py_complex) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyComplex_FromDoubles(_ real: Double, _ imag: Double) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyComplex_RealAsDouble(_ op: UnsafeMutablePointer<PyObject>!) -> Double
@discardableResult
func PyComplex_ImagAsDouble(_ op: UnsafeMutablePointer<PyObject>!) -> Double
@discardableResult
func PyComplex_AsCComplex(_ op: UnsafeMutablePointer<PyObject>!) -> Py_complex
@discardableResult
func _PyComplex_FormatAdvanced(_ obj: UnsafeMutablePointer<PyObject>!, _ format_spec: UnsafeMutablePointer<Int8>!, _ format_spec_len: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
