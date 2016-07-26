
struct NSTextListOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var prependEnclosingMarker: NSTextListOptions { get }
}
class NSTextList : NSObject, NSCoding, NSCopying {
  init(markerFormat format: String, options mask: Int)
  var markerFormat: String { get }
  var listOptions: NSTextListOptions { get }
  @discardableResult
  func marker(forItemNumber itemNum: Int) -> String
  @available(OSX 10.6, *)
  var startingItemNumber: Int
}
