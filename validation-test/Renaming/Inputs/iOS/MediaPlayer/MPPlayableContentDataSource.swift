
protocol MPPlayableContentDataSource : NSObjectProtocol {
  optional func beginLoadingChildItems(at indexPath: NSIndexPath, completionHandler completionHandler: (NSError?) -> Void)
  @discardableResult
  optional func childItemsDisplayPlaybackProgress(at indexPath: NSIndexPath) -> Bool
  @discardableResult
  func numberOfChildItems(at indexPath: NSIndexPath) -> Int
  @available(iOS 7.1, *)
  @discardableResult
  func contentItem(at indexPath: NSIndexPath) -> MPContentItem?
}
