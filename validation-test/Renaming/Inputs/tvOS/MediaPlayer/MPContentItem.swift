
@available(tvOS 7.1, *)
class MPContentItem : NSObject {
  init(identifier identifier: String)
  var identifier: String { get }
  var title: String?
  var subtitle: String?
  var artwork: MPMediaItemArtwork?
  var isContainer: Bool
  var isPlayable: Bool
  var playbackProgress: Float
}
