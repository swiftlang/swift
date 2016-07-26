
@available(OSX 10.11, *)
enum SCNBufferFrequency : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case perFrame
  case perNode
  case perShadable
}
protocol SCNBufferStream : NSObjectProtocol {
  func writeBytes(_ bytes: UnsafeMutablePointer<Void>, length length: Int)
}
typealias SCNBufferBindingBlock = (SCNBufferStream, SCNNode, SCNShadable, SCNRenderer) -> Void
typealias SCNBindingBlock = (UInt32, UInt32, SCNNode, SCNRenderer) -> Void
protocol SCNShadable : NSObjectProtocol {
  @available(OSX 10.8, *)
  optional var program: SCNProgram? { get set }
  @available(OSX 10.9, *)
  optional func handleBinding(ofSymbol symbol: String, using block: SCNBindingBlock? = nil)
  @available(OSX 10.9, *)
  optional func handleUnbinding(ofSymbol symbol: String, using block: SCNBindingBlock? = nil)
  @available(OSX 10.9, *)
  optional var shaderModifiers: [String : String]? { get set }
}
let SCNProgramMappingChannelKey: String
@available(OSX 10.8, *)
class SCNProgram : NSObject, NSCopying, NSSecureCoding {
  var vertexShader: String?
  var fragmentShader: String?
  @available(OSX 10.10, *)
  var tessellationControlShader: String?
  @available(OSX 10.10, *)
  var tessellationEvaluationShader: String?
  @available(OSX 10.10, *)
  var geometryShader: String?
  @available(OSX 10.11, *)
  var vertexFunctionName: String?
  @available(OSX 10.11, *)
  var fragmentFunctionName: String?
  @available(OSX 10.11, *)
  func handleBinding(ofBufferNamed name: String, frequency frequency: SCNBufferFrequency, using block: SCNBufferBindingBlock)
  @available(OSX 10.10, *)
  var isOpaque: Bool
  func setSemantic(_ semantic: String?, forSymbol symbol: String, options options: [String : AnyObject]? = [:])
  @discardableResult
  func semantic(forSymbol symbol: String) -> String?
  unowned(unsafe) var delegate: @sil_unmanaged SCNProgramDelegate?
  @available(OSX 10.11, *)
  var library: MTLLibrary?
}
protocol SCNProgramDelegate : NSObjectProtocol {
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  @discardableResult
  optional func program(_ program: SCNProgram, bindValueForSymbol symbol: String, atLocation location: UInt32, programID programID: UInt32, renderer renderer: SCNRenderer) -> Bool
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  optional func program(_ program: SCNProgram, unbindValueForSymbol symbol: String, atLocation location: UInt32, programID programID: UInt32, renderer renderer: SCNRenderer)
  @available(OSX 10.8, *)
  optional func program(_ program: SCNProgram, handleError error: NSError)
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  @discardableResult
  optional func programIsOpaque(_ program: SCNProgram) -> Bool
}
@available(OSX 10.9, *)
let SCNShaderModifierEntryPointGeometry: String
@available(OSX 10.9, *)
let SCNShaderModifierEntryPointSurface: String
@available(OSX 10.9, *)
let SCNShaderModifierEntryPointLightingModel: String
@available(OSX 10.9, *)
let SCNShaderModifierEntryPointFragment: String
