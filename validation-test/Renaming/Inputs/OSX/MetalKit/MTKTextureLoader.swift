
@available(OSX 10.11, *)
let MTKTextureLoaderErrorDomain: String
@available(OSX 10.11, *)
let MTKTextureLoaderErrorKey: String
@available(OSX 10.11, *)
let MTKTextureLoaderOptionAllocateMipmaps: String
@available(OSX 10.11, *)
let MTKTextureLoaderOptionSRGB: String
@available(OSX 10.11, *)
let MTKTextureLoaderOptionTextureUsage: String
@available(OSX 10.11, *)
let MTKTextureLoaderOptionTextureCPUCacheMode: String
typealias MTKTextureLoaderCallback = (MTLTexture?, NSError?) -> Void
@available(OSX 10.11, *)
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
