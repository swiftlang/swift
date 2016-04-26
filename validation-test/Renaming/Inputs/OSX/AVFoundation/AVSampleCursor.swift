
@available(OSX 10.10, *)
class AVSampleCursor : NSObject, NSCopying {
  @discardableResult
  func stepInDecodeOrder(byCount stepCount: Int64) -> Int64
  @discardableResult
  func stepInPresentationOrder(byCount stepCount: Int64) -> Int64
  @discardableResult
  func step(byDecodeTime deltaDecodeTime: CMTime, wasPinned outWasPinned: UnsafeMutablePointer<ObjCBool>?) -> CMTime
  @discardableResult
  func step(byPresentationTime deltaPresentationTime: CMTime, wasPinned outWasPinned: UnsafeMutablePointer<ObjCBool>?) -> CMTime
}
extension AVSampleCursor {
  var presentationTimeStamp: CMTime { get }
  var decodeTimeStamp: CMTime { get }
  @discardableResult
  func comparePositionInDecodeOrder(withPositionOf cursor: AVSampleCursor) -> NSComparisonResult
  @discardableResult
  func maySamplesWithEarlierDecodeTimeStampsHavePresentationTimeStamps(laterThan cursor: AVSampleCursor) -> Bool
  @discardableResult
  func maySamplesWithLaterDecodeTimeStampsHavePresentationTimeStamps(earlierThan cursor: AVSampleCursor) -> Bool
}
extension AVSampleCursor {
  var currentSampleDuration: CMTime { get }
  @discardableResult
  func copyCurrentSampleFormatDescription() -> CMFormatDescription
  var currentSampleSyncInfo: AVSampleCursorSyncInfo { get }
  var currentSampleDependencyInfo: AVSampleCursorDependencyInfo { get }
  @available(OSX 10.11, *)
  var samplesRequiredForDecoderRefresh: Int { get }
}
struct AVSampleCursorSyncInfo {
  var sampleIsFullSync: ObjCBool
  var sampleIsPartialSync: ObjCBool
  var sampleIsDroppable: ObjCBool
  init()
  init(sampleIsFullSync sampleIsFullSync: ObjCBool, sampleIsPartialSync sampleIsPartialSync: ObjCBool, sampleIsDroppable sampleIsDroppable: ObjCBool)
}
struct AVSampleCursorDependencyInfo {
  var sampleIndicatesWhetherItHasDependentSamples: ObjCBool
  var sampleHasDependentSamples: ObjCBool
  var sampleIndicatesWhetherItDependsOnOthers: ObjCBool
  var sampleDependsOnOthers: ObjCBool
  var sampleIndicatesWhetherItHasRedundantCoding: ObjCBool
  var sampleHasRedundantCoding: ObjCBool
  init()
  init(sampleIndicatesWhetherItHasDependentSamples sampleIndicatesWhetherItHasDependentSamples: ObjCBool, sampleHasDependentSamples sampleHasDependentSamples: ObjCBool, sampleIndicatesWhetherItDependsOnOthers sampleIndicatesWhetherItDependsOnOthers: ObjCBool, sampleDependsOnOthers sampleDependsOnOthers: ObjCBool, sampleIndicatesWhetherItHasRedundantCoding sampleIndicatesWhetherItHasRedundantCoding: ObjCBool, sampleHasRedundantCoding sampleHasRedundantCoding: ObjCBool)
}
extension AVSampleCursor {
  var currentChunkStorageURL: NSURL { get }
  var currentChunkStorageRange: AVSampleCursorStorageRange { get }
  var currentChunkInfo: AVSampleCursorChunkInfo { get }
  var currentSampleIndexInChunk: Int64 { get }
  var currentSampleStorageRange: AVSampleCursorStorageRange { get }
}
struct AVSampleCursorStorageRange {
  var offset: Int64
  var length: Int64
  init()
  init(offset offset: Int64, length length: Int64)
}
struct AVSampleCursorChunkInfo {
  var chunkSampleCount: Int64
  var chunkHasUniformSampleSizes: ObjCBool
  var chunkHasUniformSampleDurations: ObjCBool
  var chunkHasUniformFormatDescriptions: ObjCBool
  init()
  init(chunkSampleCount chunkSampleCount: Int64, chunkHasUniformSampleSizes chunkHasUniformSampleSizes: ObjCBool, chunkHasUniformSampleDurations chunkHasUniformSampleDurations: ObjCBool, chunkHasUniformFormatDescriptions chunkHasUniformFormatDescriptions: ObjCBool)
}
