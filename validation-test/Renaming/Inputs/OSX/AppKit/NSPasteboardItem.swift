
@available(OSX 10.6, *)
class NSPasteboardItem : NSObject, NSPasteboardWriting, NSPasteboardReading {
  var types: [String] { get }
  @discardableResult
  func availableType(from types: [String]) -> String?
  @discardableResult
  func setDataProvider(_ dataProvider: NSPasteboardItemDataProvider, forTypes types: [AnyObject]) -> Bool
  @discardableResult
  func setData(_ data: NSData?, forType type: String) -> Bool
  @discardableResult
  func setString(_ string: String?, forType type: String) -> Bool
  @discardableResult
  func setPropertyList(_ propertyList: AnyObject?, forType type: String) -> Bool
  @discardableResult
  func data(forType type: String) -> NSData?
  @discardableResult
  func string(forType type: String) -> String?
  @discardableResult
  func propertyList(forType type: String) -> AnyObject?
}
protocol NSPasteboardItemDataProvider : NSObjectProtocol {
  @available(OSX 10.6, *)
  func pasteboard(_ pasteboard: NSPasteboard?, item item: NSPasteboardItem, provideDataForType type: String)
  optional func pasteboardFinished(withDataProvider pasteboard: NSPasteboard)
}
