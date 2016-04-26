
@available(OSX 10.11, *)
class MTLVertexAttribute : NSObject {
  var name: String? { get }
  var attributeIndex: Int { get }
  @available(OSX 10.11, *)
  var attributeType: MTLDataType { get }
  var isActive: Bool { get }
}
@available(OSX 10.11, *)
enum MTLFunctionType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case vertex
  case fragment
  case kernel
}
@available(OSX 10.11, *)
protocol MTLFunction : NSObjectProtocol {
  var device: MTLDevice { get }
  var functionType: MTLFunctionType { get }
  var vertexAttributes: [MTLVertexAttribute]? { get }
  var name: String { get }
}
@available(OSX 10.11, *)
enum MTLLanguageVersion : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case version1_1
}
@available(OSX 10.11, *)
class MTLCompileOptions : NSObject, NSCopying {
  var preprocessorMacros: [String : NSObject]?
  var fastMathEnabled: Bool
  @available(OSX 10.11, *)
  var languageVersion: MTLLanguageVersion
}
@available(OSX 10.11, *)
let MTLLibraryErrorDomain: String
@available(OSX 10.11, *)
enum MTLLibraryError : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case unsupported
  case `internal`
  case compileFailure
  case compileWarning
}
let MTLRenderPipelineErrorDomain: String
@available(OSX 10.11, *)
enum MTLRenderPipelineError : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case `internal`
  case unsupported
  case invalidInput
}
@available(OSX 10.11, *)
protocol MTLLibrary : NSObjectProtocol {
  var label: String? { get set }
  var device: MTLDevice { get }
  @discardableResult
  func newFunction(withName functionName: String) -> MTLFunction?
  var functionNames: [String] { get }
}
