
@available(OSX 10.11, *)
class MDLVertexAttributeData : NSObject {
  var map: MDLMeshBufferMap
  var dataStart: UnsafeMutablePointer<Void>
  var stride: Int
  var format: MDLVertexFormat
}
@available(OSX 10.11, *)
class MDLMesh : MDLObject {
  init(vertexBuffer vertexBuffer: MDLMeshBuffer, vertexCount vertexCount: Int, descriptor descriptor: MDLVertexDescriptor, submeshes submeshes: [MDLSubmesh])
  init(vertexBuffers vertexBuffers: [MDLMeshBuffer], vertexCount vertexCount: Int, descriptor descriptor: MDLVertexDescriptor, submeshes submeshes: [MDLSubmesh])
  @discardableResult
  func vertexAttributeData(forAttributeNamed name: String) -> MDLVertexAttributeData?
  var boundingBox: MDLAxisAlignedBoundingBox { get }
  @NSCopying var vertexDescriptor: MDLVertexDescriptor
  var vertexCount: Int { get }
  var vertexBuffers: [MDLMeshBuffer] { get }
  var submeshes: NSMutableArray { get }
}
extension MDLMesh {
  func addAttribute(withName name: String, format format: MDLVertexFormat)
  func addNormals(withAttributeNamed attributeName: String?, creaseThreshold creaseThreshold: Float)
  func addTangentBasis(forTextureCoordinateAttributeNamed textureCoordinateAttributeName: String, tangentAttributeNamed tangentAttributeName: String, bitangentAttributeNamed bitangentAttributeName: String?)
  func addTangentBasis(forTextureCoordinateAttributeNamed textureCoordinateAttributeName: String, normalAttributeNamed normalAttributeName: String, tangentAttributeNamed tangentAttributeName: String)
  func makeVerticesUnique()
}
extension MDLMesh {
  @discardableResult
  class func newBox(withDimensions dimensions: vector_float3, segments segments: vector_uint3, geometryType geometryType: MDLGeometryType, inwardNormals inwardNormals: Bool, allocator allocator: MDLMeshBufferAllocator?) -> Self
  @discardableResult
  class func newEllipsoid(withRadii radii: vector_float3, radialSegments radialSegments: Int, verticalSegments verticalSegments: Int, geometryType geometryType: MDLGeometryType, inwardNormals inwardNormals: Bool, hemisphere hemisphere: Bool, allocator allocator: MDLMeshBufferAllocator?) -> Self
  @discardableResult
  class func newCylinder(withHeight height: Float, radii radii: vector_float2, radialSegments radialSegments: Int, verticalSegments verticalSegments: Int, geometryType geometryType: MDLGeometryType, inwardNormals inwardNormals: Bool, allocator allocator: MDLMeshBufferAllocator?) -> Self
  @discardableResult
  class func newEllipticalCone(withHeight height: Float, radii radii: vector_float2, radialSegments radialSegments: Int, verticalSegments verticalSegments: Int, geometryType geometryType: MDLGeometryType, inwardNormals inwardNormals: Bool, allocator allocator: MDLMeshBufferAllocator?) -> Self
  @discardableResult
  class func newPlane(withDimensions dimensions: vector_float2, segments segments: vector_uint2, geometryType geometryType: MDLGeometryType, allocator allocator: MDLMeshBufferAllocator?) -> Self
  @discardableResult
  class func newIcosahedron(withRadius radius: Float, inwardNormals inwardNormals: Bool, allocator allocator: MDLMeshBufferAllocator?) -> Self
  @discardableResult
  class func newSubdividedMesh(_ mesh: MDLMesh, submeshIndex submeshIndex: Int, subdivisionLevels subdivisionLevels: Int) -> Self?
}
extension MDLMesh {
  @discardableResult
  func generateAmbientOcclusionTexture(withSize textureSize: vector_int2, raysPerSample raysPerSample: Int, attenuationFactor attenuationFactor: Float, objectsToConsider objectsToConsider: [MDLObject], vertexAttributeNamed vertexAttributeName: String, materialPropertyNamed materialPropertyName: String) -> Bool
  @discardableResult
  func generateAmbientOcclusionTexture(withQuality bakeQuality: Float, attenuationFactor attenuationFactor: Float, objectsToConsider objectsToConsider: [MDLObject], vertexAttributeNamed vertexAttributeName: String, materialPropertyNamed materialPropertyName: String) -> Bool
  @discardableResult
  func generateAmbientOcclusionVertexColors(withRaysPerSample raysPerSample: Int, attenuationFactor attenuationFactor: Float, objectsToConsider objectsToConsider: [MDLObject], vertexAttributeNamed vertexAttributeName: String) -> Bool
  @discardableResult
  func generateAmbientOcclusionVertexColors(withQuality bakeQuality: Float, attenuationFactor attenuationFactor: Float, objectsToConsider objectsToConsider: [MDLObject], vertexAttributeNamed vertexAttributeName: String) -> Bool
  @discardableResult
  func generateLightMapTexture(withTextureSize textureSize: vector_int2, lightsToConsider lightsToConsider: [MDLLight], objectsToConsider objectsToConsider: [MDLObject], vertexAttributeNamed vertexAttributeName: String, materialPropertyNamed materialPropertyName: String) -> Bool
  @discardableResult
  func generateLightMapTexture(withQuality bakeQuality: Float, lightsToConsider lightsToConsider: [MDLLight], objectsToConsider objectsToConsider: [MDLObject], vertexAttributeNamed vertexAttributeName: String, materialPropertyNamed materialPropertyName: String) -> Bool
  @discardableResult
  func generateLightMapVertexColorsWithLights(toConsider lightsToConsider: [MDLLight], objectsToConsider objectsToConsider: [MDLObject], vertexAttributeNamed vertexAttributeName: String) -> Bool
}
