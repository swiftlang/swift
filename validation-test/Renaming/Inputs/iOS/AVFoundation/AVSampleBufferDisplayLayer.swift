
@available(iOS 8.0, *)
enum AVQueuedSampleBufferRenderingStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case rendering
  case failed
}
@available(iOS 8.0, *)
let AVSampleBufferDisplayLayerFailedToDecodeNotification: String
@available(iOS 8.0, *)
let AVSampleBufferDisplayLayerFailedToDecodeNotificationErrorKey: String
@available(iOS 8.0, *)
class AVSampleBufferDisplayLayer : CALayer {
  var controlTimebase: CMTimebase?
  var videoGravity: String
  @available(iOS 8.0, *)
  var status: AVQueuedSampleBufferRenderingStatus { get }
  @available(iOS 8.0, *)
  var error: NSError? { get }
}
extension AVSampleBufferDisplayLayer {
  func enqueue(_ sampleBuffer: CMSampleBuffer)
  func flush()
  func flushAndRemoveImage()
  var isReadyForMoreMediaData: Bool { get }
  func requestMediaDataWhenReady(on queue: dispatch_queue_t, using block: () -> Void)
  func stopRequestingMediaData()
}
