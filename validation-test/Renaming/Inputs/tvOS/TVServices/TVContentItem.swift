
@available(tvOS 9.0, *)
class TVContentItem : NSObject, NSCopying, NSSecureCoding {
  @NSCopying var contentIdentifier: TVContentIdentifier { get }
  @NSCopying var imageURL: NSURL?
  var imageShape: TVContentItemImageShape
  var title: String?
  @NSCopying var lastAccessedDate: NSDate?
  @NSCopying var expirationDate: NSDate?
  @NSCopying var creationDate: NSDate?
  @NSCopying var badgeCount: NSNumber?
  @NSCopying var duration: NSNumber?
  @NSCopying var currentPosition: NSNumber?
  @NSCopying var hasPlayedToEnd: NSNumber?
  @NSCopying var playURL: NSURL?
  @NSCopying var displayURL: NSURL?
  var topShelfItems: [TVContentItem]?
  init?(contentIdentifier ident: TVContentIdentifier)
}
@available(tvOS 9.0, *)
enum TVContentItemImageShape : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case poster
  case square
  case SDTV
  case HDTV
  case wide
  case extraWide
}
