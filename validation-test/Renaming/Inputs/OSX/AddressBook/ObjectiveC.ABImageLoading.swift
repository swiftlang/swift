
protocol ABImageClient : NSObjectProtocol {
  func consumeImageData(_ data: NSData!, forTag tag: Int)
}
extension ABPerson {
  @discardableResult
  func setImageData(_ data: NSData!) -> Bool
  @discardableResult
  func imageData() -> NSData!
  @discardableResult
  func beginLoadingImageData(for client: ABImageClient!) -> Int
  class func cancelLoadingImageData(forTag tag: Int)
}
