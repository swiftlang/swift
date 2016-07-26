
@available(OSX 10.8, *)
let GLKTextureLoaderApplyPremultiplication: String
@available(OSX 10.8, *)
let GLKTextureLoaderGenerateMipmaps: String
@available(OSX 10.8, *)
let GLKTextureLoaderOriginBottomLeft: String
@available(OSX 10.9, *)
let GLKTextureLoaderSRGB: String
@available(OSX 10.8, *)
let GLKTextureLoaderErrorDomain: String
@available(OSX 10.8, *)
let GLKTextureLoaderErrorKey: String
@available(OSX 10.8, *)
let GLKTextureLoaderGLErrorKey: String
@available(OSX 10.8, *)
enum GLKTextureLoaderError : GLuint {
  init?(rawValue rawValue: GLuint)
  var rawValue: GLuint { get }
  case fileOrURLNotFound
  case invalidNSData
  case invalidCGImage
  case unknownPathType
  case unknownFileType
  case pvrAtlasUnsupported
  case cubeMapInvalidNumFiles
  case compressedTextureUpload
  case uncompressedTextureUpload
  case unsupportedCubeMapDimensions
  case unsupportedBitDepth
  case unsupportedPVRFormat
  case dataPreprocessingFailure
  case mipmapUnsupported
  case unsupportedOrientation
  case reorientationFailure
  case alphaPremultiplicationFailure
  case invalidEAGLContext
  case incompatibleFormatSRGB
}
@available(OSX 10.8, *)
enum GLKTextureInfoAlphaState : GLint {
  init?(rawValue rawValue: GLint)
  var rawValue: GLint { get }
  case none
  case nonPremultiplied
  case premultiplied
}
@available(OSX 10.8, *)
enum GLKTextureInfoOrigin : GLint {
  init?(rawValue rawValue: GLint)
  var rawValue: GLint { get }
  case unknown
  case topLeft
  case bottomLeft
}
@available(OSX 10.8, *)
class GLKTextureInfo : NSObject, NSCopying {
  var name: GLuint { get }
  var target: GLenum { get }
  var width: GLuint { get }
  var height: GLuint { get }
  var alphaState: GLKTextureInfoAlphaState { get }
  var textureOrigin: GLKTextureInfoOrigin { get }
  var containsMipmaps: Bool { get }
}
typealias GLKTextureLoaderCallback = (GLKTextureInfo?, NSError?) -> Void
@available(OSX 10.8, *)
class GLKTextureLoader : NSObject {
  @discardableResult
  class func texture(withContentsOfFile path: String, options options: [String : NSNumber]? = [:]) throws -> GLKTextureInfo
  @discardableResult
  class func texture(withContentsOf url: NSURL, options options: [String : NSNumber]? = [:]) throws -> GLKTextureInfo
  @discardableResult
  class func texture(withContentsOf data: NSData, options options: [String : NSNumber]? = [:]) throws -> GLKTextureInfo
  @discardableResult
  class func texture(with cgImage: CGImage, options options: [String : NSNumber]? = [:]) throws -> GLKTextureInfo
  @discardableResult
  class func cubeMap(withContentsOfFiles paths: [AnyObject], options options: [String : NSNumber]? = [:]) throws -> GLKTextureInfo
  @discardableResult
  class func cubeMap(withContentsOfFile path: String, options options: [String : NSNumber]? = [:]) throws -> GLKTextureInfo
  @discardableResult
  class func cubeMap(withContentsOf url: NSURL, options options: [String : NSNumber]? = [:]) throws -> GLKTextureInfo
  init(share context: NSOpenGLContext)
  func texture(withContentsOfFile path: String, options options: [String : NSNumber]? = [:], queue queue: dispatch_queue_t?, completionHandler block: GLKTextureLoaderCallback)
  func texture(withContentsOf url: NSURL, options options: [String : NSNumber]? = [:], queue queue: dispatch_queue_t?, completionHandler block: GLKTextureLoaderCallback)
  func texture(withContentsOf data: NSData, options options: [String : NSNumber]? = [:], queue queue: dispatch_queue_t?, completionHandler block: GLKTextureLoaderCallback)
  func texture(with cgImage: CGImage, options options: [String : NSNumber]? = [:], queue queue: dispatch_queue_t?, completionHandler block: GLKTextureLoaderCallback)
  func cubeMap(withContentsOfFiles paths: [AnyObject], options options: [String : NSNumber]? = [:], queue queue: dispatch_queue_t?, completionHandler block: GLKTextureLoaderCallback)
  func cubeMap(withContentsOfFile path: String, options options: [String : NSNumber]? = [:], queue queue: dispatch_queue_t?, completionHandler block: GLKTextureLoaderCallback)
  func cubeMap(withContentsOf url: NSURL, options options: [String : NSNumber]? = [:], queue queue: dispatch_queue_t?, completionHandler block: GLKTextureLoaderCallback)
}
