
extension AVSampleBufferDisplayLayer {
  func enqueue(_ sampleBuffer: CMSampleBuffer)
  func flush()
  func flushAndRemoveImage()
  var isReadyForMoreMediaData: Bool { get }
  func requestMediaDataWhenReady(on queue: dispatch_queue_t, using block: () -> Void)
  func stopRequestingMediaData()
}
