
@available(OSX 10.11, *)
class MDLSubmeshTopology : NSObject {
  var faceTopology: MDLMeshBuffer?
  var faceCount: Int
  var vertexCreaseIndices: MDLMeshBuffer?
  var vertexCreases: MDLMeshBuffer?
  var vertexCreaseCount: Int
  var edgeCreaseIndices: MDLMeshBuffer?
  var edgeCreases: MDLMeshBuffer?
  var edgeCreaseCount: Int
  var holes: MDLMeshBuffer?
  var holeCount: Int
}
@available(OSX 10.11, *)
class MDLSubmesh : NSObject, MDLNamed {
  init(name name: String, indexBuffer indexBuffer: MDLMeshBuffer, indexCount indexCount: Int, indexType indexType: MDLIndexBitDepth, geometryType geometryType: MDLGeometryType, material material: MDLMaterial?)
  init(indexBuffer indexBuffer: MDLMeshBuffer, indexCount indexCount: Int, indexType indexType: MDLIndexBitDepth, geometryType geometryType: MDLGeometryType, material material: MDLMaterial?)
  init(name name: String, indexBuffer indexBuffer: MDLMeshBuffer, indexCount indexCount: Int, indexType indexType: MDLIndexBitDepth, geometryType geometryType: MDLGeometryType, material material: MDLMaterial?, topology topology: MDLSubmeshTopology?)
  init?(mdlSubmesh submesh: MDLSubmesh, indexType indexType: MDLIndexBitDepth, geometryType geometryType: MDLGeometryType)
  var indexBuffer: MDLMeshBuffer { get }
  var indexCount: Int { get }
  var indexType: MDLIndexBitDepth { get }
  var geometryType: MDLGeometryType { get }
  var material: MDLMaterial?
  var topology: MDLSubmeshTopology? { get }
}
