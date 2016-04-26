
@available(iOS 9.0, *)
struct MTLBlitOption : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var depthFromDepthStencil: MTLBlitOption { get }
  static var stencilFromDepthStencil: MTLBlitOption { get }
  @available(iOS 9.0, *)
  static var rowLinearPVRTC: MTLBlitOption { get }
}
@available(iOS 8.0, *)
protocol MTLBlitCommandEncoder : MTLCommandEncoder {
  func copy(from sourceTexture: MTLTexture, sourceSlice sourceSlice: Int, sourceLevel sourceLevel: Int, sourceOrigin sourceOrigin: MTLOrigin, sourceSize sourceSize: MTLSize, to destinationTexture: MTLTexture, destinationSlice destinationSlice: Int, destinationLevel destinationLevel: Int, destinationOrigin destinationOrigin: MTLOrigin)
  func copy(from sourceBuffer: MTLBuffer, sourceOffset sourceOffset: Int, sourceBytesPerRow sourceBytesPerRow: Int, sourceBytesPerImage sourceBytesPerImage: Int, sourceSize sourceSize: MTLSize, to destinationTexture: MTLTexture, destinationSlice destinationSlice: Int, destinationLevel destinationLevel: Int, destinationOrigin destinationOrigin: MTLOrigin)
  @available(iOS 9.0, *)
  func copy(from sourceBuffer: MTLBuffer, sourceOffset sourceOffset: Int, sourceBytesPerRow sourceBytesPerRow: Int, sourceBytesPerImage sourceBytesPerImage: Int, sourceSize sourceSize: MTLSize, to destinationTexture: MTLTexture, destinationSlice destinationSlice: Int, destinationLevel destinationLevel: Int, destinationOrigin destinationOrigin: MTLOrigin, options options: MTLBlitOption)
  func copy(from sourceTexture: MTLTexture, sourceSlice sourceSlice: Int, sourceLevel sourceLevel: Int, sourceOrigin sourceOrigin: MTLOrigin, sourceSize sourceSize: MTLSize, to destinationBuffer: MTLBuffer, destinationOffset destinationOffset: Int, destinationBytesPerRow destinationBytesPerRow: Int, destinationBytesPerImage destinationBytesPerImage: Int)
  @available(iOS 9.0, *)
  func copy(from sourceTexture: MTLTexture, sourceSlice sourceSlice: Int, sourceLevel sourceLevel: Int, sourceOrigin sourceOrigin: MTLOrigin, sourceSize sourceSize: MTLSize, to destinationBuffer: MTLBuffer, destinationOffset destinationOffset: Int, destinationBytesPerRow destinationBytesPerRow: Int, destinationBytesPerImage destinationBytesPerImage: Int, options options: MTLBlitOption)
  func generateMipmaps(for texture: MTLTexture)
  func fill(_ buffer: MTLBuffer, range range: NSRange, value value: UInt8)
  func copy(from sourceBuffer: MTLBuffer, sourceOffset sourceOffset: Int, to destinationBuffer: MTLBuffer, destinationOffset destinationOffset: Int, size size: Int)
}
