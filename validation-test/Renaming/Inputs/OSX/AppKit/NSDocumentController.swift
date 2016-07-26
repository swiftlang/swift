
class NSDocumentController : NSObject, NSCoding, NSUserInterfaceValidations {
  @discardableResult
  class func shared() -> NSDocumentController
  var documents: [NSDocument] { get }
  var currentDocument: NSDocument? { get }
  var currentDirectory: String? { get }
  @discardableResult
  func document(for url: NSURL) -> NSDocument?
  @discardableResult
  func document(for window: NSWindow) -> NSDocument?
  func addDocument(_ document: NSDocument)
  func removeDocument(_ document: NSDocument)
  @IBAction func newDocument(_ sender: AnyObject?)
  @discardableResult
  func openUntitledDocumentAndDisplay(_ displayDocument: Bool) throws -> NSDocument
  @discardableResult
  func makeUntitledDocument(ofType typeName: String) throws -> NSDocument
  @IBAction func openDocument(_ sender: AnyObject?)
  @discardableResult
  func urlsFromRunningOpenPanel() -> [NSURL]?
  @discardableResult
  func runModalOpenPanel(_ openPanel: NSOpenPanel, forTypes types: [String]?) -> Int
  @available(OSX 10.8, *)
  func beginOpenPanel(completionHandler completionHandler: ([NSURL]?) -> Void)
  @available(OSX 10.8, *)
  func begin(_ openPanel: NSOpenPanel, forTypes inTypes: [String]?, completionHandler completionHandler: (Int) -> Void)
  @available(OSX 10.7, *)
  func openDocument(withContentsOf url: NSURL, display displayDocument: Bool, completionHandler completionHandler: (NSDocument?, Bool, NSError?) -> Void)
  @discardableResult
  func makeDocument(withContentsOf url: NSURL, ofType typeName: String) throws -> NSDocument
  @available(OSX 10.7, *)
  func reopenDocument(for urlOrNil: NSURL?, withContentsOf contentsURL: NSURL, display displayDocument: Bool, completionHandler completionHandler: (NSDocument?, Bool, NSError?) -> Void)
  @discardableResult
  func makeDocument(for urlOrNil: NSURL?, withContentsOf contentsURL: NSURL, ofType typeName: String) throws -> NSDocument
  var autosavingDelay: NSTimeInterval
  @IBAction func saveAllDocuments(_ sender: AnyObject?)
  var hasEditedDocuments: Bool { get }
  func reviewUnsavedDocuments(withAlertTitle title: String?, cancellable cancellable: Bool, delegate delegate: AnyObject?, didReviewAllSelector didReviewAllSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  func closeAllDocuments(withDelegate delegate: AnyObject?, didCloseAllSelector didCloseAllSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @available(OSX 10.7, *)
  @discardableResult
  func duplicateDocument(withContentsOf url: NSURL, copying duplicateByCopying: Bool, displayName displayNameOrNil: String?) throws -> NSDocument
  func presentError(_ error: NSError, modalFor window: NSWindow, delegate delegate: AnyObject?, didPresent didPresentSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @discardableResult
  func presentError(_ error: NSError) -> Bool
  @discardableResult
  func willPresentError(_ error: NSError) -> NSError
  var maximumRecentDocumentCount: Int { get }
  @IBAction func clearRecentDocuments(_ sender: AnyObject?)
  func noteNewRecentDocument(_ document: NSDocument)
  func noteNewRecentDocumentURL(_ url: NSURL)
  var recentDocumentURLs: [NSURL] { get }
  var defaultType: String? { get }
  @discardableResult
  func typeForContents(of url: NSURL) throws -> String
  var documentClassNames: [String] { get }
  @discardableResult
  func documentClass(forType typeName: String) -> AnyClass?
  @discardableResult
  func displayName(forType typeName: String) -> String?
}
extension NSDocumentController {
}
