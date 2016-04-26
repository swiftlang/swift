
@available(iOS 4.0, *)
class AVAssetTrack : NSObject, NSCopying, AVAsynchronousKeyValueLoading {
  weak var asset: @sil_weak AVAsset? { get }
  var trackID: CMPersistentTrackID { get }
}
extension AVAssetTrack {
  var mediaType: String { get }
  var formatDescriptions: [AnyObject] { get }
  @available(iOS 5.0, *)
  var isPlayable: Bool { get }
  var isEnabled: Bool { get }
  var isSelfContained: Bool { get }
  var totalSampleDataLength: Int64 { get }
  @discardableResult
  func hasMediaCharacteristic(_ mediaCharacteristic: String) -> Bool
}
extension AVAssetTrack {
  var timeRange: CMTimeRange { get }
  var naturalTimeScale: CMTimeScale { get }
  var estimatedDataRate: Float { get }
}
extension AVAssetTrack {
  var languageCode: String { get }
  var extendedLanguageTag: String { get }
}
extension AVAssetTrack {
  var naturalSize: CGSize { get }
  var preferredTransform: CGAffineTransform { get }
}
extension AVAssetTrack {
  var preferredVolume: Float { get }
}
extension AVAssetTrack {
  var nominalFrameRate: Float { get }
  @available(iOS 7.0, *)
  var minFrameDuration: CMTime { get }
  @available(iOS 8.0, *)
  var requiresFrameReordering: Bool { get }
}
extension AVAssetTrack {
  var segments: [AVAssetTrackSegment] { get }
  @discardableResult
  func segment(forTrackTime trackTime: CMTime) -> AVAssetTrackSegment?
  @discardableResult
  func samplePresentationTime(forTrackTime trackTime: CMTime) -> CMTime
}
extension AVAssetTrack {
  var commonMetadata: [AVMetadataItem] { get }
  @available(iOS 8.0, *)
  var metadata: [AVMetadataItem] { get }
  var availableMetadataFormats: [String] { get }
  @discardableResult
  func metadata(forFormat format: String) -> [AVMetadataItem]
}
extension AVAssetTrack {
  @available(iOS 7.0, *)
  var availableTrackAssociationTypes: [String] { get }
  @available(iOS 7.0, *)
  @discardableResult
  func associatedTracks(ofType trackAssociationType: String) -> [AVAssetTrack]
}
@available(iOS 7.0, *)
let AVTrackAssociationTypeAudioFallback: String
@available(iOS 7.0, *)
let AVTrackAssociationTypeChapterList: String
@available(iOS 7.0, *)
let AVTrackAssociationTypeForcedSubtitlesOnly: String
@available(iOS 7.0, *)
let AVTrackAssociationTypeSelectionFollower: String
@available(iOS 7.0, *)
let AVTrackAssociationTypeTimecode: String
@available(iOS 8.0, *)
let AVTrackAssociationTypeMetadataReferent: String
@available(iOS 9.0, *)
let AVAssetTrackTimeRangeDidChangeNotification: String
@available(iOS 9.0, *)
let AVAssetTrackSegmentsDidChangeNotification: String
@available(iOS 9.0, *)
let AVAssetTrackTrackAssociationsDidChangeNotification: String
