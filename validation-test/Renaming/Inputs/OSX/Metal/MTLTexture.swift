
@available(OSX 10.11, *)
enum MTLTextureType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case type1D
  case type1DArray
  case type2D
  case type2DArray
  case type2DMultisample
  case typeCube
  @available(OSX 10.11, *)
  case typeCubeArray
  case type3D
}
@available(OSX 10.11, *)
struct MTLTextureUsage : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var shaderRead: MTLTextureUsage { get }
  static var shaderWrite: MTLTextureUsage { get }
  static var renderTarget: MTLTextureUsage { get }
  static var pixelFormatView: MTLTextureUsage { get }
}
@available(OSX 10.11, *)
class MTLTextureDescriptor : NSObject, NSCopying {
  @discardableResult
  class func texture2DDescriptor(with pixelFormat: MTLPixelFormat, width width: Int, height height: Int, mipmapped mipmapped: Bool) -> MTLTextureDescriptor
  @discardableResult
  class func textureCubeDescriptor(with pixelFormat: MTLPixelFormat, size size: Int, mipmapped mipmapped: Bool) -> MTLTextureDescriptor
  var textureType: MTLTextureType
  var pixelFormat: MTLPixelFormat
  var width: Int
  var height: Int
  var depth: Int
  var mipmapLevelCount: Int
  var sampleCount: Int
  var arrayLength: Int
  var resourceOptions: MTLResourceOptions
  @available(OSX 10.11, *)
  var cpuCacheMode: MTLCPUCacheMode
  @available(OSX 10.11, *)
  var storageMode: MTLStorageMode
  @available(OSX 10.11, *)
  var usage: MTLTextureUsage
}
@available(OSX 10.11, *)
protocol MTLTexture : MTLResource {
  var rootResource: MTLResource? { get }
  @available(OSX 10.11, *)
  var parent: MTLTexture? { get }
  @available(OSX 10.11, *)
  var parentRelativeLevel: Int { get }
  @available(OSX 10.11, *)
  var parentRelativeSlice: Int { get }
  var iosurface: IOSurface? { get }
  var iosurfacePlane: Int { get }
  var textureType: MTLTextureType { get }
  var pixelFormat: MTLPixelFormat { get }
  var width: Int { get }
  var height: Int { get }
  var depth: Int { get }
  var mipmapLevelCount: Int { get }
  var sampleCount: Int { get }
  var arrayLength: Int { get }
  var usage: MTLTextureUsage { get }
  var isFramebufferOnly: Bool { get }
  func getBytes(_ pixelBytes: UnsafeMutablePointer<Void>, bytesPerRow bytesPerRow: Int, bytesPerImage bytesPerImage: Int, from region: MTLRegion, mipmapLevel level: Int, slice slice: Int)
  func replace(_ region: MTLRegion, mipmapLevel level: Int, slice slice: Int, withBytes pixelBytes: UnsafePointer<Void>, bytesPerRow bytesPerRow: Int, bytesPerImage bytesPerImage: Int)
  func getBytes(_ pixelBytes: UnsafeMutablePointer<Void>, bytesPerRow bytesPerRow: Int, from region: MTLRegion, mipmapLevel level: Int)
  func replace(_ region: MTLRegion, mipmapLevel level: Int, withBytes pixelBytes: UnsafePointer<Void>, bytesPerRow bytesPerRow: Int)
  @discardableResult
  func newTextureView(with pixelFormat: MTLPixelFormat) -> MTLTexture
  @discardableResult
  func newTextureView(with pixelFormat: MTLPixelFormat, textureType textureType: MTLTextureType, levels levelRange: NSRange, slices sliceRange: NSRange) -> MTLTexture
}
