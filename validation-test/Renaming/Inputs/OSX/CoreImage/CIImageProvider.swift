
extension CIImage {
  @available(OSX 10.4, *)
  init(imageProvider p: AnyObject, size width: Int, _ height: Int, format f: CIFormat, colorSpace cs: CGColorSpace?, options options: [String : AnyObject]? = [:])
}
extension NSObject {
  class func provideImageData(_ data: UnsafeMutablePointer<Void>, bytesPerRow rowbytes: Int, origin x: Int, _ y: Int, size width: Int, _ height: Int, userInfo info: AnyObject?)
  func provideImageData(_ data: UnsafeMutablePointer<Void>, bytesPerRow rowbytes: Int, origin x: Int, _ y: Int, size width: Int, _ height: Int, userInfo info: AnyObject?)
}
@available(OSX 10.4, *)
let kCIImageProviderTileSize: String
@available(OSX 10.4, *)
let kCIImageProviderUserInfo: String
