
struct NSFontCollectionVisibility : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var process: NSFontCollectionVisibility { get }
  static var user: NSFontCollectionVisibility { get }
  static var computer: NSFontCollectionVisibility { get }
}
@available(OSX 10.7, *)
class NSFontCollection : NSObject, NSCopying, NSMutableCopying, NSCoding {
  /*not inherited*/ init(descriptors queryDescriptors: [NSFontDescriptor])
  @discardableResult
  class func withAllAvailableDescriptors() -> NSFontCollection
  /*not inherited*/ init?(locale locale: NSLocale)
  class func show(_ collection: NSFontCollection, withName name: String, visibility visibility: NSFontCollectionVisibility) throws
  class func hide(withName name: String, visibility visibility: NSFontCollectionVisibility) throws
  class func renameFontCollection(withName name: String, visibility visibility: NSFontCollectionVisibility, toName name: String) throws
  @discardableResult
  class func allFontCollectionNames() -> [String]
  /*not inherited*/ init?(name name: String)
  /*not inherited*/ init?(name name: String, visibility visibility: NSFontCollectionVisibility)
  var queryDescriptors: [NSFontDescriptor]? { get }
  var exclusionDescriptors: [NSFontDescriptor]? { get }
  var matchingDescriptors: [NSFontDescriptor]? { get }
  @discardableResult
  func matchingDescriptors(options options: [String : NSNumber]? = [:]) -> [NSFontDescriptor]?
  @discardableResult
  func matchingDescriptors(forFamily family: String) -> [NSFontDescriptor]?
  @discardableResult
  func matchingDescriptors(forFamily family: String, options options: [String : NSNumber]? = [:]) -> [NSFontDescriptor]?
}
@available(OSX 10.7, *)
class NSMutableFontCollection : NSFontCollection {
  func addQuery(for descriptors: [NSFontDescriptor])
  func removeQuery(for descriptors: [NSFontDescriptor])
}
@available(OSX 10.7, *)
let NSFontCollectionIncludeDisabledFontsOption: String
@available(OSX 10.7, *)
let NSFontCollectionRemoveDuplicatesOption: String
@available(OSX 10.7, *)
let NSFontCollectionDisallowAutoActivationOption: String
@available(OSX 10.7, *)
let NSFontCollectionDidChangeNotification: String
@available(OSX 10.7, *)
let NSFontCollectionActionKey: String
@available(OSX 10.7, *)
let NSFontCollectionNameKey: String
@available(OSX 10.7, *)
let NSFontCollectionOldNameKey: String
@available(OSX 10.7, *)
let NSFontCollectionVisibilityKey: String
@available(OSX 10.7, *)
let NSFontCollectionWasShown: String
@available(OSX 10.7, *)
let NSFontCollectionWasHidden: String
@available(OSX 10.7, *)
let NSFontCollectionWasRenamed: String
@available(OSX 10.7, *)
let NSFontCollectionAllFonts: String
@available(OSX 10.7, *)
let NSFontCollectionUser: String
@available(OSX 10.7, *)
let NSFontCollectionFavorites: String
@available(OSX 10.7, *)
let NSFontCollectionRecentlyUsed: String
