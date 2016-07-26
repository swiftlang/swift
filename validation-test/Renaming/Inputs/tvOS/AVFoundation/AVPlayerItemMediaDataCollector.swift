
@available(tvOS 9.3, *)
class AVPlayerItemMediaDataCollector : NSObject {
}
@available(tvOS 9.3, *)
class AVPlayerItemMetadataCollector : AVPlayerItemMediaDataCollector {
  init(identifiers identifiers: [String]?, classifyingLabels classifyingLabels: [String]?)
  func setDelegate(_ delegate: AVPlayerItemMetadataCollectorPushDelegate?, queue delegateQueue: dispatch_queue_t?)
  weak var delegate: @sil_weak AVPlayerItemMetadataCollectorPushDelegate? { get }
  var delegateQueue: dispatch_queue_t? { get }
}
protocol AVPlayerItemMetadataCollectorPushDelegate : NSObjectProtocol {
  @available(tvOS 9.3, *)
  func metadataCollector(_ metadataCollector: AVPlayerItemMetadataCollector, didCollect metadataGroups: [AVDateRangeMetadataGroup], indexesOfNewGroups indexesOfNewGroups: NSIndexSet, indexesOfModifiedGroups indexesOfModifiedGroups: NSIndexSet)
}
