
protocol NSFilePresenter : NSObjectProtocol {
  @NSCopying var presentedItemURL: NSURL? { get }
  @available(OSX 10.5, *)
  var presentedItemOperationQueue: NSOperationQueue { get }
  @available(OSX 10.8, *)
  @NSCopying optional var primaryPresentedItemURL: NSURL? { get }
  optional func relinquishPresentedItem(toReader reader: ((() -> Void)?) -> Void)
  optional func relinquishPresentedItem(toWriter writer: ((() -> Void)?) -> Void)
  optional func savePresentedItemChanges(completionHandler completionHandler: (NSError?) -> Void)
  optional func accommodatePresentedItemDeletion(completionHandler completionHandler: (NSError?) -> Void)
  optional func presentedItemDidMove(to newURL: NSURL)
  optional func presentedItemDidChange()
  @available(OSX 10.7, *)
  optional func presentedItemDidGain(_ version: NSFileVersion)
  @available(OSX 10.7, *)
  optional func presentedItemDidLose(_ version: NSFileVersion)
  @available(OSX 10.7, *)
  optional func presentedItemDidResolveConflict(_ version: NSFileVersion)
  optional func accommodatePresentedSubitemDeletion(at url: NSURL, completionHandler completionHandler: (NSError?) -> Void)
  optional func presentedSubitemDidAppear(at url: NSURL)
  optional func presentedSubitem(at oldURL: NSURL, didMoveTo newURL: NSURL)
  optional func presentedSubitemDidChange(at url: NSURL)
  @available(OSX 10.7, *)
  optional func presentedSubitem(at url: NSURL, didGain version: NSFileVersion)
  @available(OSX 10.7, *)
  optional func presentedSubitem(at url: NSURL, didLose version: NSFileVersion)
  @available(OSX 10.7, *)
  optional func presentedSubitem(at url: NSURL, didResolve version: NSFileVersion)
}
