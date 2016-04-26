
@available(iOS 3.0, *)
struct MPMediaType : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var music: MPMediaType { get }
  static var podcast: MPMediaType { get }
  static var audioBook: MPMediaType { get }
  @available(iOS 5.0, *)
  static var audioITunesU: MPMediaType { get }
  static var anyAudio: MPMediaType { get }
  @available(iOS 5.0, *)
  static var movie: MPMediaType { get }
  @available(iOS 5.0, *)
  static var tvShow: MPMediaType { get }
  @available(iOS 5.0, *)
  static var videoPodcast: MPMediaType { get }
  @available(iOS 5.0, *)
  static var musicVideo: MPMediaType { get }
  @available(iOS 5.0, *)
  static var videoITunesU: MPMediaType { get }
  @available(iOS 7.0, *)
  static var homeVideo: MPMediaType { get }
  @available(iOS 5.0, *)
  static var anyVideo: MPMediaType { get }
  static var any: MPMediaType { get }
}
@available(iOS 3.0, *)
class MPMediaItem : MPMediaEntity {
  @available(iOS 7.0, *)
  var mediaType: MPMediaType { get }
  @available(iOS 7.0, *)
  var title: String? { get }
  @available(iOS 7.0, *)
  var albumTitle: String? { get }
  @available(iOS 8.0, *)
  var albumPersistentID: MPMediaEntityPersistentID { get }
  @available(iOS 7.0, *)
  var artist: String? { get }
  @available(iOS 8.0, *)
  var artistPersistentID: MPMediaEntityPersistentID { get }
  @available(iOS 7.0, *)
  var albumArtist: String? { get }
  @available(iOS 8.0, *)
  var albumArtistPersistentID: MPMediaEntityPersistentID { get }
  @available(iOS 7.0, *)
  var genre: String? { get }
  @available(iOS 8.0, *)
  var genrePersistentID: MPMediaEntityPersistentID { get }
  @available(iOS 7.0, *)
  var composer: String? { get }
  @available(iOS 8.0, *)
  var composerPersistentID: MPMediaEntityPersistentID { get }
  @available(iOS 7.0, *)
  var playbackDuration: NSTimeInterval { get }
  @available(iOS 7.0, *)
  var albumTrackNumber: Int { get }
  @available(iOS 8.0, *)
  var albumTrackCount: Int { get }
  @available(iOS 7.0, *)
  var discNumber: Int { get }
  @available(iOS 8.0, *)
  var discCount: Int { get }
  @available(iOS 7.0, *)
  var artwork: MPMediaItemArtwork? { get }
  @available(iOS 8.0, *)
  var lyrics: String? { get }
  @available(iOS 8.0, *)
  var isCompilation: Bool { get }
  @available(iOS 7.0, *)
  var releaseDate: NSDate? { get }
  @available(iOS 8.0, *)
  var beatsPerMinute: Int { get }
  @available(iOS 8.0, *)
  var comments: String? { get }
  @available(iOS 8.0, *)
  var assetURL: NSURL? { get }
  @available(iOS 8.0, *)
  var isCloudItem: Bool { get }
  @available(iOS 9.2, *)
  var hasProtectedAsset: Bool { get }
  @available(iOS 7.0, *)
  var podcastTitle: String? { get }
  @available(iOS 8.0, *)
  var podcastPersistentID: MPMediaEntityPersistentID { get }
  @available(iOS 7.0, *)
  var playCount: Int { get }
  @available(iOS 7.0, *)
  var skipCount: Int { get }
  @available(iOS 7.0, *)
  var rating: Int { get }
  @available(iOS 7.0, *)
  var lastPlayedDate: NSDate? { get }
  @available(iOS 8.0, *)
  var userGrouping: String? { get }
  @available(iOS 7.0, *)
  var bookmarkTime: NSTimeInterval { get }
}
@available(iOS 4.2, *)
let MPMediaItemPropertyPersistentID: String
let MPMediaItemPropertyMediaType: String
let MPMediaItemPropertyTitle: String
let MPMediaItemPropertyAlbumTitle: String
@available(iOS 4.2, *)
let MPMediaItemPropertyAlbumPersistentID: String
let MPMediaItemPropertyArtist: String
@available(iOS 4.2, *)
let MPMediaItemPropertyArtistPersistentID: String
let MPMediaItemPropertyAlbumArtist: String
@available(iOS 4.2, *)
let MPMediaItemPropertyAlbumArtistPersistentID: String
let MPMediaItemPropertyGenre: String
@available(iOS 4.2, *)
let MPMediaItemPropertyGenrePersistentID: String
let MPMediaItemPropertyComposer: String
@available(iOS 4.2, *)
let MPMediaItemPropertyComposerPersistentID: String
let MPMediaItemPropertyPlaybackDuration: String
let MPMediaItemPropertyAlbumTrackNumber: String
let MPMediaItemPropertyAlbumTrackCount: String
let MPMediaItemPropertyDiscNumber: String
let MPMediaItemPropertyDiscCount: String
let MPMediaItemPropertyArtwork: String
let MPMediaItemPropertyLyrics: String
let MPMediaItemPropertyIsCompilation: String
@available(iOS 4.0, *)
let MPMediaItemPropertyReleaseDate: String
@available(iOS 4.0, *)
let MPMediaItemPropertyBeatsPerMinute: String
@available(iOS 4.0, *)
let MPMediaItemPropertyComments: String
@available(iOS 4.0, *)
let MPMediaItemPropertyAssetURL: String
@available(iOS 6.0, *)
let MPMediaItemPropertyIsCloudItem: String
@available(iOS 9.2, *)
let MPMediaItemPropertyHasProtectedAsset: String
let MPMediaItemPropertyPodcastTitle: String
@available(iOS 4.2, *)
let MPMediaItemPropertyPodcastPersistentID: String
let MPMediaItemPropertyPlayCount: String
let MPMediaItemPropertySkipCount: String
let MPMediaItemPropertyRating: String
let MPMediaItemPropertyLastPlayedDate: String
@available(iOS 4.0, *)
let MPMediaItemPropertyUserGrouping: String
@available(iOS 6.0, *)
let MPMediaItemPropertyBookmarkTime: String
@available(iOS 3.0, *)
class MPMediaItemArtwork : NSObject {
  @available(iOS 5.0, *)
  init(image image: UIImage)
  @discardableResult
  func image(with size: CGSize) -> UIImage?
  var bounds: CGRect { get }
  var imageCropRect: CGRect { get }
}
