
@available(OSX 10.10, *)
enum AVQueuedSampleBufferRenderingStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case rendering
  case failed
}
@available(OSX 10.10, *)
let AVSampleBufferDisplayLayerFailedToDecodeNotification: String
@available(OSX 10.10, *)
let AVSampleBufferDisplayLayerFailedToDecodeNotificationErrorKey: String
@available(OSX 10.8, *)
class AVSampleBufferDisplayLayer : CALayer {
  var controlTimebase: CMTimebase?
  var videoGravity: String
  @available(OSX 10.10, *)
  var status: AVQueuedSampleBufferRenderingStatus { get }
  @available(OSX 10.10, *)
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
