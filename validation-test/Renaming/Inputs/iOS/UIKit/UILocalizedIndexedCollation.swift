
@available(iOS 3.0, *)
class UILocalizedIndexedCollation : NSObject {
  @discardableResult
  class func current() -> Self
  var sectionTitles: [String] { get }
  var sectionIndexTitles: [String] { get }
  @discardableResult
  func sectionForSectionIndexTitle(at indexTitleIndex: Int) -> Int
  @discardableResult
  func section(for object: AnyObject, collationStringSelector selector: Selector) -> Int
  @discardableResult
  func sortedArray(from array: [AnyObject], collationStringSelector selector: Selector) -> [AnyObject]
}
