
@available(tvOS 9.0, *)
let MTKTextureLoaderErrorDomain: String
@available(tvOS 9.0, *)
let MTKTextureLoaderErrorKey: String
@available(tvOS 9.0, *)
let MTKTextureLoaderOptionAllocateMipmaps: String
@available(tvOS 9.0, *)
let MTKTextureLoaderOptionSRGB: String
@available(tvOS 9.0, *)
let MTKTextureLoaderOptionTextureUsage: String
@available(tvOS 9.0, *)
let MTKTextureLoaderOptionTextureCPUCacheMode: String
typealias MTKTextureLoaderCallback = (MTLTexture?, NSError?) -> Void
@available(tvOS 9.0, *)
class MTKTextureLoader : NSObject {
  var device: MTLDevice { get }
  init(device device: MTLDevice)
  func newTexture(withContentsOf URL: NSURL, options options: [String : NSNumber]? = [:], completionHandler completionHandler: MTKTextureLoaderCallback)
  func newTexture(with data: NSData, options options: [String : NSNumber]? = [:], completionHandler completionHandler: MTKTextureLoaderCallback)
  func newTexture(with cgImage: CGImage, options options: [String : NSNumber]? = [:], completionHandler completionHandler: MTKTextureLoaderCallback)
  @discardableResult
  func newTexture(withContentsOf URL: NSURL, options options: [String : NSNumber]? = [:]) throws -> MTLTexture
  @discardableResult
  func newTexture(with data: NSData, options options: [String : NSNumber]? = [:]) throws -> MTLTexture
  @discardableResult
  func newTexture(with cgImage: CGImage, options options: [String : NSNumber]? = [:]) throws -> MTLTexture
}
