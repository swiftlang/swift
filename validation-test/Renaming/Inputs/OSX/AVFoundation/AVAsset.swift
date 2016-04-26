
@available(OSX 10.7, *)
class AVAsset : NSObject, NSCopying, AVAsynchronousKeyValueLoading {
  convenience init(url URL: NSURL)
  var duration: CMTime { get }
  var preferredRate: Float { get }
  var preferredVolume: Float { get }
  var preferredTransform: CGAffineTransform { get }
}
extension AVAsset {
  var providesPreciseDurationAndTiming: Bool { get }
  func cancelLoading()
}
extension AVAsset {
  @available(OSX 10.7, *)
  var referenceRestrictions: AVAssetReferenceRestrictions { get }
}
struct AVAssetReferenceRestrictions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var forbidRemoteReferenceToLocal: AVAssetReferenceRestrictions { get }
  static var forbidLocalReferenceToRemote: AVAssetReferenceRestrictions { get }
  static var forbidCrossSiteReference: AVAssetReferenceRestrictions { get }
  static var forbidLocalReferenceToLocal: AVAssetReferenceRestrictions { get }
  static var forbidAll: AVAssetReferenceRestrictions { get }
}
extension AVAsset {
  var tracks: [AVAssetTrack] { get }
  @discardableResult
  func track(withTrackID trackID: CMPersistentTrackID) -> AVAssetTrack?
  @discardableResult
  func tracks(withMediaType mediaType: String) -> [AVAssetTrack]
  @discardableResult
  func tracks(withMediaCharacteristic mediaCharacteristic: String) -> [AVAssetTrack]
  @available(OSX 10.9, *)
  var trackGroups: [AVAssetTrackGroup] { get }
}
extension AVAsset {
  @available(OSX 10.8, *)
  var creationDate: AVMetadataItem? { get }
  var lyrics: String? { get }
  var commonMetadata: [AVMetadataItem] { get }
  @available(OSX 10.10, *)
  var metadata: [AVMetadataItem] { get }
  var availableMetadataFormats: [String] { get }
  @discardableResult
  func metadata(forFormat format: String) -> [AVMetadataItem]
}
extension AVAsset {
  @available(OSX 10.7, *)
  var availableChapterLocales: [NSLocale] { get }
  @available(OSX 10.7, *)
  @discardableResult
  func chapterMetadataGroups(withTitleLocale locale: NSLocale, containingItemsWithCommonKeys commonKeys: [String]?) -> [AVTimedMetadataGroup]
  @available(OSX 10.8, *)
  @discardableResult
  func chapterMetadataGroups(bestMatchingPreferredLanguages preferredLanguages: [String]) -> [AVTimedMetadataGroup]
}
extension AVAsset {
  @available(OSX 10.8, *)
  var availableMediaCharacteristicsWithMediaSelectionOptions: [String] { get }
  @available(OSX 10.8, *)
  @discardableResult
  func mediaSelectionGroup(forMediaCharacteristic mediaCharacteristic: String) -> AVMediaSelectionGroup?
  @available(OSX 10.11, *)
  var preferredMediaSelection: AVMediaSelection { get }
}
extension AVAsset {
  @available(OSX 10.7, *)
  var hasProtectedContent: Bool { get }
}
extension AVAsset {
  @available(OSX 10.11, *)
  var canContainFragments: Bool { get }
  @available(OSX 10.11, *)
  var containsFragments: Bool { get }
}
extension AVAsset {
  @available(OSX 10.7, *)
  var isPlayable: Bool { get }
  @available(OSX 10.7, *)
  var isExportable: Bool { get }
  @available(OSX 10.7, *)
  var isReadable: Bool { get }
  @available(OSX 10.7, *)
  var isComposable: Bool { get }
  @available(OSX 10.11, *)
  var isCompatibleWithAirPlayVideo: Bool { get }
}
@available(OSX 10.7, *)
let AVURLAssetPreferPreciseDurationAndTimingKey: String
@available(OSX 10.7, *)
let AVURLAssetReferenceRestrictionsKey: String
@available(OSX 10.7, *)
class AVURLAsset : AVAsset {
  @available(OSX 10.7, *)
  @discardableResult
  class func audiovisualTypes() -> [String]
  @available(OSX 10.7, *)
  @discardableResult
  class func audiovisualMIMETypes() -> [String]
  @available(OSX 10.7, *)
  @discardableResult
  class func isPlayableExtendedMIMEType(_ extendedMIMEType: String) -> Bool
  init(url URL: NSURL, options options: [String : AnyObject]? = [:])
  @NSCopying var url: NSURL { get }
}
extension AVURLAsset {
  @available(OSX 10.9, *)
  var resourceLoader: AVAssetResourceLoader { get }
}
extension AVURLAsset {
  @discardableResult
  func compatibleTrack(for compositionTrack: AVCompositionTrack) -> AVAssetTrack?
}
@available(OSX 10.11, *)
let AVAssetDurationDidChangeNotification: String
@available(OSX 10.11, *)
let AVAssetContainsFragmentsDidChangeNotification: String
@available(OSX 10.11, *)
let AVAssetWasDefragmentedNotification: String
@available(OSX 10.11, *)
let AVAssetChapterMetadataGroupsDidChangeNotification: String
@available(OSX 10.11, *)
let AVAssetMediaSelectionGroupsDidChangeNotification: String
protocol AVFragmentMinding {
  @available(OSX 10.11, *)
  var isAssociatedWithFragmentMinder: Bool { get }
}
@available(OSX 10.11, *)
class AVFragmentedAsset : AVURLAsset, AVFragmentMinding {
}
extension AVFragmentedAsset {
}
@available(OSX 10.11, *)
class AVFragmentedAssetMinder : NSObject {
  convenience init(asset asset: AVAsset, mindingInterval mindingInterval: NSTimeInterval)
  var mindingInterval: NSTimeInterval
  var assets: [AVAsset] { get }
  func addFragmentedAsset(_ asset: AVAsset)
  func removeFragmentedAsset(_ asset: AVAsset)
}
