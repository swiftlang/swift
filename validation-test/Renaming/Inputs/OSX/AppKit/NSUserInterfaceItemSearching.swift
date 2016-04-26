
protocol NSUserInterfaceItemSearching : NSObjectProtocol {
  func searchForItems(withSearch searchString: String, resultLimit resultLimit: Int, matchedItemHandler handleMatchedItems: ([AnyObject]) -> Void)
  @discardableResult
  func localizedTitles(forItem item: AnyObject) -> [String]
  optional func performAction(forItem item: AnyObject)
  optional func showAllHelpTopics(forSearch searchString: String)
}
extension NSApplication {
  @available(OSX 10.6, *)
  func registerUserInterfaceItemSearchHandler(_ handler: NSUserInterfaceItemSearching)
  @available(OSX 10.6, *)
  func unregisterUserInterfaceItemSearchHandler(_ handler: NSUserInterfaceItemSearching)
  @available(OSX 10.6, *)
  @discardableResult
  func search(_ searchString: String, inUserInterfaceItemString stringToSearch: String, search searchRange: NSRange, found foundRange: UnsafeMutablePointer<NSRange>?) -> Bool
}
