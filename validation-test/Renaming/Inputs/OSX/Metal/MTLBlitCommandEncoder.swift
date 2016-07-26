
@available(OSX 10.11, *)
struct MTLBlitOption : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var depthFromDepthStencil: MTLBlitOption { get }
  static var stencilFromDepthStencil: MTLBlitOption { get }
}
@available(OSX 10.11, *)
protocol MTLBlitCommandEncoder : MTLCommandEncoder {
  @available(OSX 10.11, *)
  func synchronizeResource(_ resource: MTLResource)
  @available(OSX 10.11, *)
  func synchronizeTexture(_ texture: MTLTexture, slice slice: Int, level level: Int)
  func copy(from sourceTexture: MTLTexture, sourceSlice sourceSlice: Int, sourceLevel sourceLevel: Int, sourceOrigin sourceOrigin: MTLOrigin, sourceSize sourceSize: MTLSize, to destinationTexture: MTLTexture, destinationSlice destinationSlice: Int, destinationLevel destinationLevel: Int, destinationOrigin destinationOrigin: MTLOrigin)
  func copy(from sourceBuffer: MTLBuffer, sourceOffset sourceOffset: Int, sourceBytesPerRow sourceBytesPerRow: Int, sourceBytesPerImage sourceBytesPerImage: Int, sourceSize sourceSize: MTLSize, to destinationTexture: MTLTexture, destinationSlice destinationSlice: Int, destinationLevel destinationLevel: Int, destinationOrigin destinationOrigin: MTLOrigin)
  @available(OSX 10.11, *)
  func copy(from sourceBuffer: MTLBuffer, sourceOffset sourceOffset: Int, sourceBytesPerRow sourceBytesPerRow: Int, sourceBytesPerImage sourceBytesPerImage: Int, sourceSize sourceSize: MTLSize, to destinationTexture: MTLTexture, destinationSlice destinationSlice: Int, destinationLevel destinationLevel: Int, destinationOrigin destinationOrigin: MTLOrigin, options options: MTLBlitOption)
  func copy(from sourceTexture: MTLTexture, sourceSlice sourceSlice: Int, sourceLevel sourceLevel: Int, sourceOrigin sourceOrigin: MTLOrigin, sourceSize sourceSize: MTLSize, to destinationBuffer: MTLBuffer, destinationOffset destinationOffset: Int, destinationBytesPerRow destinationBytesPerRow: Int, destinationBytesPerImage destinationBytesPerImage: Int)
  @available(OSX 10.11, *)
  func copy(from sourceTexture: MTLTexture, sourceSlice sourceSlice: Int, sourceLevel sourceLevel: Int, sourceOrigin sourceOrigin: MTLOrigin, sourceSize sourceSize: MTLSize, to destinationBuffer: MTLBuffer, destinationOffset destinationOffset: Int, destinationBytesPerRow destinationBytesPerRow: Int, destinationBytesPerImage destinationBytesPerImage: Int, options options: MTLBlitOption)
  func generateMipmaps(for texture: MTLTexture)
  func fill(_ buffer: MTLBuffer, range range: NSRange, value value: UInt8)
  func copy(from sourceBuffer: MTLBuffer, sourceOffset sourceOffset: Int, to destinationBuffer: MTLBuffer, destinationOffset destinationOffset: Int, size size: Int)
}
