
@available(tvOS 9.0, *)
class MPSUnaryImageKernel : MPSKernel {
  var offset: MPSOffset
  var clipRect: MTLRegion
  var edgeMode: MPSImageEdgeMode
  @discardableResult
  func encode(to commandBuffer: MTLCommandBuffer, inPlace texture: UnsafeMutablePointer<MTLTexture>, fallbackCopyAllocator copyAllocator: MPSCopyAllocator? = nil) -> Bool
  func encode(to commandBuffer: MTLCommandBuffer, sourceTexture sourceTexture: MTLTexture, destinationTexture destinationTexture: MTLTexture)
  @discardableResult
  func sourceRegion(forDestinationSize destinationSize: MTLSize) -> MPSRegion
}
@available(tvOS 9.0, *)
class MPSBinaryImageKernel : MPSKernel {
  var primaryOffset: MPSOffset
  var secondaryOffset: MPSOffset
  var primaryEdgeMode: MPSImageEdgeMode
  var secondaryEdgeMode: MPSImageEdgeMode
  var clipRect: MTLRegion
  @discardableResult
  func encode(to commandBuffer: MTLCommandBuffer, primaryTexture primaryTexture: MTLTexture, inPlaceSecondaryTexture inPlaceSecondaryTexture: UnsafeMutablePointer<MTLTexture>, fallbackCopyAllocator copyAllocator: MPSCopyAllocator? = nil) -> Bool
  @discardableResult
  func encode(to commandBuffer: MTLCommandBuffer, inPlacePrimaryTexture inPlacePrimaryTexture: UnsafeMutablePointer<MTLTexture>, secondaryTexture secondaryTexture: MTLTexture, fallbackCopyAllocator copyAllocator: MPSCopyAllocator? = nil) -> Bool
  func encode(to commandBuffer: MTLCommandBuffer, primaryTexture primaryTexture: MTLTexture, secondaryTexture secondaryTexture: MTLTexture, destinationTexture destinationTexture: MTLTexture)
  @discardableResult
  func primarySourceRegion(forDestinationSize destinationSize: MTLSize) -> MPSRegion
  @discardableResult
  func secondarySourceRegion(forDestinationSize destinationSize: MTLSize) -> MPSRegion
}
