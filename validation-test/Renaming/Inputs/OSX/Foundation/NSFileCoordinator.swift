
struct NSFileCoordinatorReadingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var withoutChanges: NSFileCoordinatorReadingOptions { get }
  static var resolvesSymbolicLink: NSFileCoordinatorReadingOptions { get }
  @available(OSX 10.10, *)
  static var immediatelyAvailableMetadataOnly: NSFileCoordinatorReadingOptions { get }
  @available(OSX 10.10, *)
  static var forUploading: NSFileCoordinatorReadingOptions { get }
}
struct NSFileCoordinatorWritingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var forDeleting: NSFileCoordinatorWritingOptions { get }
  static var forMoving: NSFileCoordinatorWritingOptions { get }
  static var forMerging: NSFileCoordinatorWritingOptions { get }
  static var forReplacing: NSFileCoordinatorWritingOptions { get }
  @available(OSX 10.10, *)
  static var contentIndependentMetadataOnly: NSFileCoordinatorWritingOptions { get }
}
@available(OSX 10.10, *)
class NSFileAccessIntent : NSObject {
  @discardableResult
  class func readingIntent(with url: NSURL, options options: NSFileCoordinatorReadingOptions = []) -> Self
  @discardableResult
  class func writingIntent(with url: NSURL, options options: NSFileCoordinatorWritingOptions = []) -> Self
  @NSCopying var url: NSURL { get }
}
@available(OSX 10.7, *)
class NSFileCoordinator : NSObject {
  class func addFilePresenter(_ filePresenter: NSFilePresenter)
  class func removeFilePresenter(_ filePresenter: NSFilePresenter)
  @discardableResult
  class func filePresenters() -> [NSFilePresenter]
  init(filePresenter filePresenterOrNil: NSFilePresenter?)
  @available(OSX 10.7, *)
  var purposeIdentifier: String
  @available(OSX 10.10, *)
  func coordinate(with intents: [NSFileAccessIntent], queue queue: NSOperationQueue, byAccessor accessor: (NSError?) -> Void)
  func coordinate(readingItemAt url: NSURL, options options: NSFileCoordinatorReadingOptions = [], error outError: NSErrorPointer, byAccessor reader: (NSURL) -> Void)
  func coordinate(writingItemAt url: NSURL, options options: NSFileCoordinatorWritingOptions = [], error outError: NSErrorPointer, byAccessor writer: (NSURL) -> Void)
  func coordinate(readingItemAt readingURL: NSURL, options readingOptions: NSFileCoordinatorReadingOptions = [], writingItemAt writingURL: NSURL, options writingOptions: NSFileCoordinatorWritingOptions = [], error outError: NSErrorPointer, byAccessor readerWriter: (NSURL, NSURL) -> Void)
  func coordinate(writingItemAt url1: NSURL, options options1: NSFileCoordinatorWritingOptions = [], writingItemAt url2: NSURL, options options2: NSFileCoordinatorWritingOptions = [], error outError: NSErrorPointer, byAccessor writer: (NSURL, NSURL) -> Void)
  func prepare(forReadingItemsAt readingURLs: [NSURL], options readingOptions: NSFileCoordinatorReadingOptions = [], writingItemsAt writingURLs: [NSURL], options writingOptions: NSFileCoordinatorWritingOptions = [], error outError: NSErrorPointer, byAccessor batchAccessor: (() -> Void) -> Void)
  @available(OSX 10.8, *)
  func item(at oldURL: NSURL, willMoveTo newURL: NSURL)
  func item(at oldURL: NSURL, didMoveTo newURL: NSURL)
  func cancel()
}
