
typealias MDLVoxelIndex = vector_int4
struct MDLVoxelIndexExtent {
  var minimumExtent: MDLVoxelIndex
  var maximumExtent: MDLVoxelIndex
  init()
  init(minimumExtent minimumExtent: MDLVoxelIndex, maximumExtent maximumExtent: MDLVoxelIndex)
}
@available(iOS 9.0, *)
class MDLVoxelArray : NSObject {
  init(asset asset: MDLAsset, divisions divisions: Int32, interiorShells interiorShells: Int32, exteriorShells exteriorShells: Int32, patchRadius patchRadius: Float)
  init(asset asset: MDLAsset, divisions divisions: Int32, interiorNBWidth interiorNBWidth: Float, exteriorNBWidth exteriorNBWidth: Float, patchRadius patchRadius: Float)
  init(data voxelData: NSData, boundingBox boundingBox: MDLAxisAlignedBoundingBox, voxelExtent voxelExtent: Float)
  @discardableResult
  func mesh(using allocator: MDLMeshBufferAllocator?) -> MDLMesh?
  @discardableResult
  func voxelExists(atIndex index: MDLVoxelIndex, allowAnyX allowAnyX: Bool, allowAnyY allowAnyY: Bool, allowAnyZ allowAnyZ: Bool, allowAnyShell allowAnyShell: Bool) -> Bool
  func setVoxelAtIndex(_ index: MDLVoxelIndex)
  func setVoxelsFor(_ mesh: MDLMesh, divisions divisions: Int32, interiorShells interiorShells: Int32, exteriorShells exteriorShells: Int32, patchRadius patchRadius: Float)
  func setVoxelsFor(_ mesh: MDLMesh, divisions divisions: Int32, interiorNBWidth interiorNBWidth: Float, exteriorNBWidth exteriorNBWidth: Float, patchRadius patchRadius: Float)
  @discardableResult
  func voxels(within extent: MDLVoxelIndexExtent) -> NSData?
  @discardableResult
  func voxelIndices() -> NSData?
  func union(with voxels: MDLVoxelArray)
  func difference(with voxels: MDLVoxelArray)
  func intersect(with voxels: MDLVoxelArray)
  @discardableResult
  func index(ofSpatialLocation location: vector_float3) -> MDLVoxelIndex
  @discardableResult
  func spatialLocation(ofIndex index: MDLVoxelIndex) -> vector_float3
  @discardableResult
  func voxelBoundingBox(atIndex index: MDLVoxelIndex) -> MDLAxisAlignedBoundingBox
  var count: Int { get }
  var voxelIndexExtent: MDLVoxelIndexExtent { get }
  var boundingBox: MDLAxisAlignedBoundingBox { get }
}
