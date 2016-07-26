
@available(iOS 8.0, *)
enum PHImageContentMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case aspectFit
  case aspectFill
  static var `default`: PHImageContentMode { get }
}
@available(iOS 8.0, *)
enum PHCollectionListType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case momentList
  case folder
  case smartFolder
}
@available(iOS 8.0, *)
enum PHCollectionListSubtype : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case momentListCluster
  case momentListYear
  case regularFolder
  case smartFolderEvents
  case smartFolderFaces
  case any
}
@available(iOS 8.0, *)
enum PHCollectionEditOperation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case deleteContent
  case removeContent
  case addContent
  case createContent
  case rearrangeContent
  case delete
  case rename
}
@available(iOS 8.0, *)
enum PHAssetCollectionType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case album
  case smartAlbum
  case moment
}
@available(iOS 8.0, *)
enum PHAssetCollectionSubtype : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case albumRegular
  case albumSyncedEvent
  case albumSyncedFaces
  case albumSyncedAlbum
  case albumImported
  case albumMyPhotoStream
  case albumCloudShared
  case smartAlbumGeneric
  case smartAlbumPanoramas
  case smartAlbumVideos
  case smartAlbumFavorites
  case smartAlbumTimelapses
  case smartAlbumAllHidden
  case smartAlbumRecentlyAdded
  case smartAlbumBursts
  case smartAlbumSlomoVideos
  case smartAlbumUserLibrary
  @available(iOS 9.0, *)
  case smartAlbumSelfPortraits
  @available(iOS 9.0, *)
  case smartAlbumScreenshots
  case any
}
@available(iOS 8.0, *)
enum PHAssetEditOperation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case delete
  case content
  case properties
}
@available(iOS 8.0, *)
enum PHAssetMediaType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case image
  case video
  case audio
}
@available(iOS 8.0, *)
struct PHAssetMediaSubtype : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var photoPanorama: PHAssetMediaSubtype { get }
  static var photoHDR: PHAssetMediaSubtype { get }
  @available(iOS 9.0, *)
  static var photoScreenshot: PHAssetMediaSubtype { get }
  @available(iOS 9.1, *)
  static var photoLive: PHAssetMediaSubtype { get }
  static var videoStreamed: PHAssetMediaSubtype { get }
  static var videoHighFrameRate: PHAssetMediaSubtype { get }
  static var videoTimelapse: PHAssetMediaSubtype { get }
}
@available(iOS 8.0, *)
struct PHAssetBurstSelectionType : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var autoPick: PHAssetBurstSelectionType { get }
  static var userPick: PHAssetBurstSelectionType { get }
}
@available(iOS 9.0, *)
struct PHAssetSourceType : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var typeUserLibrary: PHAssetSourceType { get }
  static var typeCloudShared: PHAssetSourceType { get }
  static var typeiTunesSynced: PHAssetSourceType { get }
}
@available(iOS 9.0, *)
enum PHAssetResourceType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case photo
  case video
  case audio
  case alternatePhoto
  case fullSizePhoto
  case fullSizeVideo
  case adjustmentData
  case adjustmentBasePhoto
  @available(iOS 9.1, *)
  case pairedVideo
}
