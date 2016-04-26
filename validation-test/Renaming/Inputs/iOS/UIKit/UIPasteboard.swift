
let UIPasteboardNameGeneral: String
let UIPasteboardNameFind: String
@available(iOS 3.0, *)
class UIPasteboard : NSObject {
  @discardableResult
  class func general() -> UIPasteboard
  /*not inherited*/ init?(name pasteboardName: String, create create: Bool)
  @discardableResult
  class func withUniqueName() -> UIPasteboard
  var name: String { get }
  class func remove(withName pasteboardName: String)
  var isPersistent: Bool
  var changeCount: Int { get }
  @discardableResult
  func pasteboardTypes() -> [String]
  @discardableResult
  func containsPasteboardTypes(_ pasteboardTypes: [String]) -> Bool
  @discardableResult
  func data(forPasteboardType pasteboardType: String) -> NSData?
  @discardableResult
  func value(forPasteboardType pasteboardType: String) -> AnyObject?
  func setValue(_ value: AnyObject, forPasteboardType pasteboardType: String)
  func setData(_ data: NSData, forPasteboardType pasteboardType: String)
  var numberOfItems: Int { get }
  @discardableResult
  func pasteboardTypes(forItemSet itemSet: NSIndexSet?) -> [AnyObject]?
  @discardableResult
  func containsPasteboardTypes(_ pasteboardTypes: [String], inItemSet itemSet: NSIndexSet?) -> Bool
  @discardableResult
  func itemSet(withPasteboardTypes pasteboardTypes: [AnyObject]) -> NSIndexSet?
  @discardableResult
  func values(forPasteboardType pasteboardType: String, inItemSet itemSet: NSIndexSet?) -> [AnyObject]?
  @discardableResult
  func data(forPasteboardType pasteboardType: String, inItemSet itemSet: NSIndexSet?) -> [AnyObject]?
  var items: [AnyObject]
  func addItems(_ items: [[String : AnyObject]])
}
let UIPasteboardChangedNotification: String
let UIPasteboardChangedTypesAddedKey: String
let UIPasteboardChangedTypesRemovedKey: String
let UIPasteboardRemovedNotification: String
var UIPasteboardTypeListString: NSArray
var UIPasteboardTypeListURL: NSArray
var UIPasteboardTypeListImage: NSArray
var UIPasteboardTypeListColor: NSArray
extension UIPasteboard {
  var string: String?
  var strings: [String]?
  @NSCopying var url: NSURL?
  var urls: [NSURL]?
  @NSCopying var image: UIImage?
  var images: [UIImage]?
  @NSCopying var color: UIColor?
  var colors: [UIColor]?
}
