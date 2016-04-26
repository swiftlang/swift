
extension CSSearchableItemAttributeSet {
  var displayName: String?
  var alternateNames: [String]?
  var path: String?
  var contentURL: NSURL?
  var thumbnailURL: NSURL?
  @NSCopying var thumbnailData: NSData?
  var relatedUniqueIdentifier: String?
  var metadataModificationDate: NSDate?
  var contentType: String?
  var contentTypeTree: [String]?
  var keywords: [String]?
  var title: String?
}
extension CSSearchableItemAttributeSet {
  @NSCopying var supportsPhoneCall: NSNumber?
  @NSCopying var supportsNavigation: NSNumber?
}
extension CSSearchableItemAttributeSet {
  var containerTitle: String?
  var containerDisplayName: String?
  var containerIdentifier: String?
  @NSCopying var containerOrder: NSNumber?
}
