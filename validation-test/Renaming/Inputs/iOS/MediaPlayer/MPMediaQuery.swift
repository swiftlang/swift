
@available(iOS 3.0, *)
enum MPMediaGrouping : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case title
  case album
  case artist
  case albumArtist
  case composer
  case genre
  case playlist
  case podcastTitle
}
@available(iOS 3.0, *)
class MPMediaQuery : NSObject, NSSecureCoding, NSCopying {
  init(filterPredicates filterPredicates: Set<MPMediaPredicate>?)
  var filterPredicates: Set<MPMediaPredicate>?
  func addFilterPredicate(_ predicate: MPMediaPredicate)
  func removeFilterPredicate(_ predicate: MPMediaPredicate)
  var items: [MPMediaItem]? { get }
  var collections: [MPMediaItemCollection]? { get }
  var groupingType: MPMediaGrouping
  @available(iOS 4.2, *)
  var itemSections: [MPMediaQuerySection]? { get }
  @available(iOS 4.2, *)
  var collectionSections: [MPMediaQuerySection]? { get }
  @discardableResult
  class func albums() -> MPMediaQuery
  @discardableResult
  class func artists() -> MPMediaQuery
  @discardableResult
  class func songs() -> MPMediaQuery
  @discardableResult
  class func playlists() -> MPMediaQuery
  @discardableResult
  class func podcasts() -> MPMediaQuery
  @discardableResult
  class func audiobooks() -> MPMediaQuery
  @discardableResult
  class func compilations() -> MPMediaQuery
  @discardableResult
  class func composers() -> MPMediaQuery
  @discardableResult
  class func genres() -> MPMediaQuery
}
@available(iOS 3.0, *)
class MPMediaPredicate : NSObject, NSSecureCoding {
}
@available(iOS 3.0, *)
enum MPMediaPredicateComparison : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case equalTo
  case contains
}
@available(iOS 3.0, *)
class MPMediaPropertyPredicate : MPMediaPredicate {
  /*not inherited*/ init(value value: AnyObject?, forProperty property: String)
  /*not inherited*/ init(value value: AnyObject?, forProperty property: String, comparisonType comparisonType: MPMediaPredicateComparison)
  var property: String { get }
  @NSCopying var value: AnyObject? { get }
  var comparisonType: MPMediaPredicateComparison { get }
}
extension MPMediaItem {
  @available(iOS 4.2, *)
  @discardableResult
  class func persistentIDProperty(forGroupingType groupingType: MPMediaGrouping) -> String
  @available(iOS 4.2, *)
  @discardableResult
  class func titleProperty(forGroupingType groupingType: MPMediaGrouping) -> String
}
