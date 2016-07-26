
protocol NSFilePresenter : NSObjectProtocol {
  @NSCopying var presentedItemURL: NSURL? { get }
  @available(tvOS 2.0, *)
  var presentedItemOperationQueue: NSOperationQueue { get }
  optional func relinquishPresentedItem(toReader reader: ((() -> Void)?) -> Void)
  optional func relinquishPresentedItem(toWriter writer: ((() -> Void)?) -> Void)
  optional func savePresentedItemChanges(completionHandler completionHandler: (NSError?) -> Void)
  optional func accommodatePresentedItemDeletion(completionHandler completionHandler: (NSError?) -> Void)
  optional func presentedItemDidMove(to newURL: NSURL)
  optional func presentedItemDidChange()
  @available(tvOS 5.0, *)
  optional func presentedItemDidGain(_ version: NSFileVersion)
  @available(tvOS 5.0, *)
  optional func presentedItemDidLose(_ version: NSFileVersion)
  @available(tvOS 5.0, *)
  optional func presentedItemDidResolveConflict(_ version: NSFileVersion)
  optional func accommodatePresentedSubitemDeletion(at url: NSURL, completionHandler completionHandler: (NSError?) -> Void)
  optional func presentedSubitemDidAppear(at url: NSURL)
  optional func presentedSubitem(at oldURL: NSURL, didMoveTo newURL: NSURL)
  optional func presentedSubitemDidChange(at url: NSURL)
  @available(tvOS 5.0, *)
  optional func presentedSubitem(at url: NSURL, didGain version: NSFileVersion)
  @available(tvOS 5.0, *)
  optional func presentedSubitem(at url: NSURL, didLose version: NSFileVersion)
  @available(tvOS 5.0, *)
  optional func presentedSubitem(at url: NSURL, didResolve version: NSFileVersion)
}
