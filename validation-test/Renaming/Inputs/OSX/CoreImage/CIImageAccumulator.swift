
@available(OSX 10.4, *)
class CIImageAccumulator : NSObject {
  init(extent extent: CGRect, format format: CIFormat)
  @available(OSX 10.7, *)
  init(extent extent: CGRect, format format: CIFormat, colorSpace colorSpace: CGColorSpace)
  var extent: CGRect { get }
  var format: CIFormat { get }
  @discardableResult
  func image() -> CIImage
  func setImage(_ image: CIImage)
  func setImage(_ image: CIImage, dirtyRect dirtyRect: CGRect)
  func clear()
}
