
@available(tvOS 6.0, *)
class AVPlayerItemOutput : NSObject {
  @discardableResult
  func itemTime(forHostTime hostTimeInSeconds: CFTimeInterval) -> CMTime
  @discardableResult
  func itemTime(forMachAbsoluteTime machAbsoluteTime: Int64) -> CMTime
  @available(tvOS 6.0, *)
  var suppressesPlayerRendering: Bool
}
@available(tvOS 6.0, *)
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
  @available(tvOS 6.0, *)
  optional func outputMediaDataWillChange(_ sender: AVPlayerItemOutput)
  @available(tvOS 6.0, *)
  optional func outputSequenceWasFlushed(_ output: AVPlayerItemOutput)
}
@available(tvOS 7.0, *)
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
@available(tvOS 7.0, *)
let AVPlayerItemLegibleOutputTextStylingResolutionDefault: String
@available(tvOS 7.0, *)
let AVPlayerItemLegibleOutputTextStylingResolutionSourceAndRulesOnly: String
protocol AVPlayerItemLegibleOutputPushDelegate : AVPlayerItemOutputPushDelegate {
  @available(tvOS 7.0, *)
  optional func legibleOutput(_ output: AVPlayerItemLegibleOutput, didOutputAttributedStrings strings: [NSAttributedString], nativeSampleBuffers nativeSamples: [AnyObject], forItemTime itemTime: CMTime)
}
protocol AVPlayerItemOutputPushDelegate : NSObjectProtocol {
  @available(tvOS 6.0, *)
  optional func outputSequenceWasFlushed(_ output: AVPlayerItemOutput)
}
@available(tvOS 8.0, *)
class AVPlayerItemMetadataOutput : AVPlayerItemOutput {
  init(identifiers identifiers: [String]?)
  func setDelegate(_ delegate: AVPlayerItemMetadataOutputPushDelegate?, queue delegateQueue: dispatch_queue_t?)
  weak var delegate: @sil_weak AVPlayerItemMetadataOutputPushDelegate? { get }
  var delegateQueue: dispatch_queue_t? { get }
  var advanceIntervalForDelegateInvocation: NSTimeInterval
}
protocol AVPlayerItemMetadataOutputPushDelegate : AVPlayerItemOutputPushDelegate {
  @available(tvOS 8.0, *)
  optional func metadataOutput(_ output: AVPlayerItemMetadataOutput, didOutputTimedMetadataGroups groups: [AVTimedMetadataGroup], from track: AVPlayerItemTrack)
}
