
@available(OSX 10.10, *)
class AVSampleBufferGenerator : NSObject {
  init(asset asset: AVAsset, timebase timebase: CMTimebase?)
  @discardableResult
  func createSampleBuffer(for request: AVSampleBufferRequest) -> CMSampleBuffer
  class func notifyOfDataReady(for sbuf: CMSampleBuffer, completionHandler completionHandler: (Bool, NSError) -> Void)
}
enum AVSampleBufferRequestDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case forward
  case none
  case reverse
}
enum AVSampleBufferRequestMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case immediate
  case scheduled
}
@available(OSX 10.10, *)
class AVSampleBufferRequest : NSObject {
  init(start startCursor: AVSampleCursor)
  var startCursor: AVSampleCursor { get }
  var direction: AVSampleBufferRequestDirection
  var limitCursor: AVSampleCursor?
  var preferredMinSampleCount: Int
  var maxSampleCount: Int
  var mode: AVSampleBufferRequestMode
  var overrideTime: CMTime
}
