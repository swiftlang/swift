
@available(tvOS 7.0, *)
enum SKTextureFilteringMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case nearest
  case linear
}
class SKTexture : NSObject, NSCopying, NSCoding {
  convenience init(imageNamed name: String)
  convenience init(rect rect: CGRect, in texture: SKTexture)
  @available(tvOS 8.0, *)
  convenience init(vectorNoiseWithSmoothness smoothness: CGFloat, size size: CGSize)
  @available(tvOS 8.0, *)
  convenience init(noiseWithSmoothness smoothness: CGFloat, size size: CGSize, grayscale grayscale: Bool)
  convenience init(cgImage image: CGImage)
  convenience init(image image: UIImage)
  convenience init(data pixelData: NSData, size size: CGSize)
  convenience init(data pixelData: NSData, size size: CGSize, flipped flipped: Bool)
  convenience init(data pixelData: NSData, size size: CGSize, rowLength rowLength: UInt32, alignment alignment: UInt32)
  @discardableResult
  func applying(_ filter: CIFilter) -> Self
  @available(tvOS 8.0, *)
  @discardableResult
  func generatingNormalMap() -> Self
  @available(tvOS 8.0, *)
  @discardableResult
  func generatingNormalMap(withSmoothness smoothness: CGFloat, contrast contrast: CGFloat) -> Self
  @discardableResult
  func textureRect() -> CGRect
  @discardableResult
  func size() -> CGSize
  var filteringMode: SKTextureFilteringMode
  var usesMipmaps: Bool
  @available(tvOS 9.0, *)
  @discardableResult
  func cgImage() -> CGImage
  class func preload(_ textures: [SKTexture], withCompletionHandler completionHandler: () -> Void)
  func preload(completionHandler completionHandler: () -> Void)
}

extension SKTexture : CustomPlaygroundQuickLookable {
}
