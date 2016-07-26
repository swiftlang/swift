
struct NSFileVersionAddingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var byMoving: NSFileVersionAddingOptions { get }
}
struct NSFileVersionReplacingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var byMoving: NSFileVersionReplacingOptions { get }
}
@available(iOS 5.0, *)
class NSFileVersion : NSObject {
  @discardableResult
  class func currentVersionOfItem(at url: NSURL) -> NSFileVersion?
  @discardableResult
  class func otherVersionsOfItem(at url: NSURL) -> [NSFileVersion]?
  @discardableResult
  class func unresolvedConflictVersionsOfItem(at url: NSURL) -> [NSFileVersion]?
  @available(iOS 8.0, *)
  class func getNonlocalVersionsOfItem(at url: NSURL, completionHandler completionHandler: ([NSFileVersion]?, NSError?) -> Void)
  /*not inherited*/ init?(itemAt url: NSURL, forPersistentIdentifier persistentIdentifier: AnyObject)
  @NSCopying var url: NSURL { get }
  var localizedName: String? { get }
  var localizedNameOfSavingComputer: String? { get }
  @NSCopying var modificationDate: NSDate? { get }
  var persistentIdentifier: NSCoding { get }
  var isConflict: Bool { get }
  var isResolved: Bool
  @available(iOS 8.0, *)
  var hasLocalContents: Bool { get }
  @available(iOS 8.0, *)
  var hasThumbnail: Bool { get }
  @discardableResult
  func replaceItem(at url: NSURL, options options: NSFileVersionReplacingOptions = []) throws -> NSURL
  func remove() throws
  class func removeOtherVersionsOfItem(at url: NSURL) throws
}
