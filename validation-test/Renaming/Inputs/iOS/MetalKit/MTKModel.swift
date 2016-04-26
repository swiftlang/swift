
@available(iOS 9.0, *)
let MTKModelErrorDomain: String
@available(iOS 9.0, *)
let MTKModelErrorKey: String
@available(iOS 9.0, *)
class MTKMeshBufferAllocator : NSObject, MDLMeshBufferAllocator {
  init(device device: MTLDevice)
  var device: MTLDevice { get }
}
@available(iOS 9.0, *)
class MTKMeshBuffer : NSObject, MDLMeshBuffer {
  var buffer: MTLBuffer { get }
  var offset: Int { get }
}
@available(iOS 9.0, *)
class MTKSubmesh : NSObject {
  var primitiveType: MTLPrimitiveType { get }
  var indexType: MTLIndexType { get }
  var indexBuffer: MTKMeshBuffer { get }
  var indexCount: Int { get }
  weak var mesh: @sil_weak MTKMesh? { get }
  var name: String
}
@available(iOS 9.0, *)
class MTKMesh : NSObject {
  init(mesh mesh: MDLMesh, device device: MTLDevice) throws
  @discardableResult
  class func newMeshes(from asset: MDLAsset, device device: MTLDevice, sourceMeshes sourceMeshes: AutoreleasingUnsafeMutablePointer<NSArray?>?) throws -> [MTKMesh]
  var vertexBuffers: [MTKMeshBuffer] { get }
  var vertexDescriptor: MDLVertexDescriptor { get }
  var submeshes: [MTKSubmesh] { get }
  var vertexCount: Int { get }
  var name: String
}
@discardableResult
func MTKModelIOVertexDescriptorFromMetal(_ metalDescriptor: MTLVertexDescriptor) -> MDLVertexDescriptor
@discardableResult
func MTKMetalVertexDescriptorFromModelIO(_ modelIODescriptor: MDLVertexDescriptor) -> MTLVertexDescriptor
@discardableResult
func MTKModelIOVertexFormatFromMetal(_ vertexFormat: MTLVertexFormat) -> MDLVertexFormat
@discardableResult
func MTKMetalVertexFormatFromModelIO(_ vertexFormat: MDLVertexFormat) -> MTLVertexFormat
