
@available(tvOS 8.0, *)
class SKMutableTexture : SKTexture {
  init(size size: CGSize)
  init(size size: CGSize, pixelFormat format: Int32)
  func modifyPixelData(_ block: (UnsafeMutablePointer<Void>!, Int) -> Void)
}
