
@available(OSX 10.10, *)
let AVMovieReferenceRestrictionsKey: String
@available(OSX 10.10, *)
class AVMovie : AVAsset, NSCopying, NSMutableCopying {
  @discardableResult
  class func movieTypes() -> [String]
  init(url URL: NSURL, options options: [String : AnyObject]? = [:])
  @available(OSX 10.11, *)
  init(data data: NSData, options options: [String : AnyObject]? = [:])
  var url: NSURL? { get }
  @available(OSX 10.11, *)
  var data: NSData? { get }
  @available(OSX 10.11, *)
  var defaultMediaDataStorage: AVMediaDataStorage? { get }
  var canContainMovieFragments: Bool { get }
  @available(OSX 10.11, *)
  var containsMovieFragments: Bool { get }
}
@available(OSX 10.11, *)
struct AVMovieWritingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var truncateDestinationToMovieHeaderOnly: AVMovieWritingOptions { get }
}
extension AVMovie {
  @available(OSX 10.11, *)
  @discardableResult
  func movieHeader(withFileType fileType: String) throws -> NSData
  @available(OSX 10.11, *)
  func writeHeader(to URL: NSURL, fileType fileType: String, options options: AVMovieWritingOptions = []) throws
  @available(OSX 10.11, *)
  @discardableResult
  func isCompatibleWithFileType(_ fileType: String) -> Bool
}
extension AVMovie {
}
@available(OSX 10.11, *)
class AVMutableMovie : AVMovie {
  init(url URL: NSURL, options options: [String : AnyObject]? = [:], error error: ()) throws
  init(data data: NSData, options options: [String : AnyObject]? = [:], error error: ()) throws
  init(settingsFrom movie: AVMovie?, options options: [String : AnyObject]? = [:]) throws
  var timescale: CMTimeScale
}
extension AVMutableMovie {
  var isModified: Bool
  var interleavingPeriod: CMTime
  func insert(_ timeRange: CMTimeRange, of asset: AVAsset, at startTime: CMTime, copySampleData copySampleData: Bool) throws
  func insertEmpty(_ timeRange: CMTimeRange)
  func remove(_ timeRange: CMTimeRange)
  func scaleTimeRange(_ timeRange: CMTimeRange, toDuration duration: CMTime)
}
extension AVMutableMovie {
  @discardableResult
  func mutableTrack(compatibleWith track: AVAssetTrack) -> AVMutableMovieTrack?
  @discardableResult
  func addMutableTrack(withMediaType mediaType: String, copySettingsFrom track: AVAssetTrack?, options options: [String : AnyObject]? = [:]) -> AVMutableMovieTrack
  @discardableResult
  func addMutableTracksCopyingSettings(from existingTracks: [AVAssetTrack], options options: [String : AnyObject]? = [:]) -> [AVMutableMovieTrack]
  func removeTrack(_ track: AVMovieTrack)
}
extension AVMutableMovie {
}
extension AVMutableMovie {
}
@available(OSX 10.11, *)
class AVMediaDataStorage : NSObject {
  init(url URL: NSURL, options options: [String : AnyObject]? = [:])
  @discardableResult
  func url() -> NSURL?
}
@available(OSX 10.10, *)
let AVFragmentedMovieContainsMovieFragmentsDidChangeNotification: String
@available(OSX 10.10, *)
let AVFragmentedMovieDurationDidChangeNotification: String
@available(OSX 10.10, *)
let AVFragmentedMovieWasDefragmentedNotification: String
@available(OSX 10.10, *)
class AVFragmentedMovie : AVMovie, AVFragmentMinding {
}
extension AVFragmentedMovie {
}
@available(OSX 10.10, *)
class AVFragmentedMovieMinder : AVFragmentedAssetMinder {
  init(movie movie: AVFragmentedMovie, mindingInterval mindingInterval: NSTimeInterval)
  var movies: [AVFragmentedMovie] { get }
  func add(_ movie: AVFragmentedMovie)
  func remove(_ movie: AVFragmentedMovie)
}
