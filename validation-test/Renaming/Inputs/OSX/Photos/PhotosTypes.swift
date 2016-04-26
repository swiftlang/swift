
@available(OSX 10.11, *)
enum PHAssetMediaType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case image
  case video
  case audio
}
@available(OSX 10.11, *)
struct PHAssetMediaSubtype : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var photoPanorama: PHAssetMediaSubtype { get }
  static var photoHDR: PHAssetMediaSubtype { get }
  static var photoScreenshot: PHAssetMediaSubtype { get }
  static var videoStreamed: PHAssetMediaSubtype { get }
  static var videoHighFrameRate: PHAssetMediaSubtype { get }
  static var videoTimelapse: PHAssetMediaSubtype { get }
}
