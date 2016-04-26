
@available(OSX 10.11, *)
let kGLKModelErrorDomain: String
@available(OSX 10.11, *)
let kGLKModelErrorKey: String
@available(OSX 10.11, *)
class GLKMeshBufferAllocator : NSObject, MDLMeshBufferAllocator {
}
@available(OSX 10.11, *)
class GLKMeshBuffer : NSObject, MDLMeshBuffer {
  var glBufferName: GLuint { get }
  var offset: Int { get }
}
@available(OSX 10.11, *)
class GLKSubmesh : NSObject {
  var type: GLenum { get }
  var mode: GLenum { get }
  var elementCount: GLsizei { get }
  var elementBuffer: GLKMeshBuffer { get }
  weak var mesh: @sil_weak GLKMesh? { get }
  var name: String { get }
}
@available(OSX 10.11, *)
class GLKMesh : NSObject {
  init(mesh mesh: MDLMesh) throws
  @discardableResult
  class func newMeshes(from asset: MDLAsset, sourceMeshes sourceMeshes: AutoreleasingUnsafeMutablePointer<NSArray?>?) throws -> [GLKMesh]
  var vertexCount: Int { get }
  var vertexBuffers: [GLKMeshBuffer] { get }
  var vertexDescriptor: MDLVertexDescriptor { get }
  var submeshes: [GLKSubmesh] { get }
  var name: String { get }
}
struct _GLKVertexAttributeParameters {
  var type: GLenum
  var size: GLint
  var normalized: GLboolean
  init()
  init(type type: GLenum, size size: GLint, normalized normalized: GLboolean)
}
typealias GLKVertexAttributeParameters = _GLKVertexAttributeParameters
@discardableResult
func GLKVertexAttributeParametersFromModelIO(_ vertexFormat: MDLVertexFormat) -> GLKVertexAttributeParameters
