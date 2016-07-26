
@available(iOS 8.0, *)
class MTLVertexAttribute : NSObject {
  var name: String? { get }
  var attributeIndex: Int { get }
  @available(iOS 8.3, *)
  var attributeType: MTLDataType { get }
  var isActive: Bool { get }
}
@available(iOS 8.0, *)
enum MTLFunctionType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case vertex
  case fragment
  case kernel
}
@available(iOS 8.0, *)
protocol MTLFunction : NSObjectProtocol {
  var device: MTLDevice { get }
  var functionType: MTLFunctionType { get }
  var vertexAttributes: [MTLVertexAttribute]? { get }
  var name: String { get }
}
@available(iOS 9.0, *)
enum MTLLanguageVersion : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  @available(iOS 9.0, *)
  case version1_0
  case version1_1
}
@available(iOS 8.0, *)
class MTLCompileOptions : NSObject, NSCopying {
  var preprocessorMacros: [String : NSObject]?
  var fastMathEnabled: Bool
  @available(iOS 9.0, *)
  var languageVersion: MTLLanguageVersion
}
@available(iOS 8.0, *)
let MTLLibraryErrorDomain: String
@available(iOS 8.0, *)
enum MTLLibraryError : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case unsupported
  case `internal`
  case compileFailure
  case compileWarning
}
let MTLRenderPipelineErrorDomain: String
@available(iOS 8.0, *)
enum MTLRenderPipelineError : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case `internal`
  case unsupported
  case invalidInput
}
@available(iOS 8.0, *)
protocol MTLLibrary : NSObjectProtocol {
  var label: String? { get set }
  var device: MTLDevice { get }
  @discardableResult
  func newFunction(withName functionName: String) -> MTLFunction?
  var functionNames: [String] { get }
}
