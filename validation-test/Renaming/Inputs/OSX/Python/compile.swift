
@discardableResult
func PyNode_Compile(_ _: UnsafeMutablePointer<_node>!, _ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyCodeObject>!
struct PyFutureFeatures {
  var ff_features: Int32
  var ff_lineno: Int32
  init()
  init(ff_features ff_features: Int32, ff_lineno ff_lineno: Int32)
}
var FUTURE_NESTED_SCOPES: String { get }
var FUTURE_GENERATORS: String { get }
var FUTURE_DIVISION: String { get }
var FUTURE_ABSOLUTE_IMPORT: String { get }
var FUTURE_WITH_STATEMENT: String { get }
var FUTURE_PRINT_FUNCTION: String { get }
var FUTURE_UNICODE_LITERALS: String { get }
@discardableResult
func PyAST_Compile(_ _: OpaquePointer!, _ _: UnsafePointer<Int8>!, _ _: UnsafeMutablePointer<PyCompilerFlags>!, _ _: OpaquePointer!) -> UnsafeMutablePointer<PyCodeObject>!
@discardableResult
func PyFuture_FromAST(_ _: OpaquePointer!, _ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyFutureFeatures>!
