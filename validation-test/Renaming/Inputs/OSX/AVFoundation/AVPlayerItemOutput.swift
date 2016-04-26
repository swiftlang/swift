
@available(OSX 10.8, *)
class AVPlayerItemOutput : NSObject {
  @discardableResult
  func itemTime(forHostTime hostTimeInSeconds: CFTimeInterval) -> CMTime
  @discardableResult
  func itemTime(forMachAbsoluteTime machAbsoluteTime: Int64) -> CMTime
  @available(OSX 10.8, *)
  @discardableResult
  func itemTime(for timestamp: CVTimeStamp) -> CMTime
  @available(OSX 10.8, *)
  var suppressesPlayerRendering: Bool
}
@available(OSX 10.8, *)
class AVPlayerItemVideoOutput : AVPlayerItemOutput {
  init(pixelBufferAttributes pixelBufferAttributes: [String : AnyObject]? = [:])
  @discardableResult
  func hasNewPixelBuffer(forItemTime itemTime: CMTime) -> Bool
  @discardableResult
  func copyPixelBuffer(forItemTime itemTime: CMTime, itemTimeForDisplay outItemTimeForDisplay: UnsafeMutablePointer<CMTime>?) -> CVPixelBuffer?
  func setDelegate(_ delegate: AVPlayerItemOutputPullDelegate?, queue delegateQueue: dispatch_queue_t?)
  func requestNotificationOfMediaDataChange(withAdvanceInterval interval: NSTimeInterval)
  unowned(unsafe) var delegate: @sil_unmanaged AVPlayerItemOutputPullDelegate? { get }
  var delegateQueue: dispatch_queue_t? { get }
}
protocol AVPlayerItemOutputPullDelegate : NSObjectProtocol {
  @available(OSX 10.8, *)
  optional func outputMediaDataWillChange(_ sender: AVPlayerItemOutput)
  @available(OSX 10.8, *)
  optional func outputSequenceWasFlushed(_ output: AVPlayerItemOutput)
}
@available(OSX 10.9, *)
class AVPlayerItemLegibleOutput : AVPlayerItemOutput {
  func setDelegate(_ delegate: AVPlayerItemLegibleOutputPushDelegate?, queue delegateQueue: dispatch_queue_t?)
  weak var delegate: @sil_weak AVPlayerItemLegibleOutputPushDelegate? { get }
  var delegateQueue: dispatch_queue_t? { get }
  var advanceIntervalForDelegateInvocation: NSTimeInterval
}
extension AVPlayerItemLegibleOutput {
  init(mediaSubtypesForNativeRepresentation subtypes: [NSNumber])
}
extension AVPlayerItemLegibleOutput {
  var textStylingResolution: String
}
@available(OSX 10.9, *)
let AVPlayerItemLegibleOutputTextStylingResolutionDefault: String
@available(OSX 10.9, *)
let AVPlayerItemLegibleOutputTextStylingResolutionSourceAndRulesOnly: String
protocol AVPlayerItemLegibleOutputPushDelegate : AVPlayerItemOutputPushDelegate {
  @available(OSX 10.9, *)
  optional func legibleOutput(_ output: AVPlayerItemLegibleOutput, didOutputAttributedStrings strings: [NSAttributedString], nativeSampleBuffers nativeSamples: [AnyObject], forItemTime itemTime: CMTime)
}
protocol AVPlayerItemOutputPushDelegate : NSObjectProtocol {
  @available(OSX 10.8, *)
  optional func outputSequenceWasFlushed(_ output: AVPlayerItemOutput)
}
@available(OSX 10.10, *)
class AVPlayerItemMetadataOutput : AVPlayerItemOutput {
  init(identifiers identifiers: [String]?)
  func setDelegate(_ delegate: AVPlayerItemMetadataOutputPushDelegate?, queue delegateQueue: dispatch_queue_t?)
  weak var delegate: @sil_weak AVPlayerItemMetadataOutputPushDelegate? { get }
  var delegateQueue: dispatch_queue_t? { get }
  var advanceIntervalForDelegateInvocation: NSTimeInterval
}
protocol AVPlayerItemMetadataOutputPushDelegate : AVPlayerItemOutputPushDelegate {
  @available(OSX 10.10, *)
  optional func metadataOutput(_ output: AVPlayerItemMetadataOutput, didOutputTimedMetadataGroups groups: [AVTimedMetadataGroup], from track: AVPlayerItemTrack)
}
