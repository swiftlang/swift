
enum NSDocumentChangeType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case changeDone
  case changeUndone
  @available(OSX 10.5, *)
  case changeRedone
  case changeCleared
  case changeReadOtherContents
  case changeAutosaved
  @available(OSX 10.7, *)
  case changeDiscardable
}
enum NSSaveOperationType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case saveOperation
  case saveAsOperation
  case saveToOperation
  @available(OSX 10.7, *)
  case autosaveInPlaceOperation
  @available(OSX 10.7, *)
  case autosaveElsewhereOperation
  @available(OSX 10.8, *)
  case autosaveAsOperation
}
class NSDocument : NSObject, NSFilePresenter, NSUserInterfaceValidations {
  convenience init(type typeName: String) throws
  @available(OSX 10.6, *)
  @discardableResult
  class func canConcurrentlyReadDocuments(ofType typeName: String) -> Bool
  convenience init(contentsOf url: NSURL, ofType typeName: String) throws
  convenience init(for urlOrNil: NSURL?, withContentsOf contentsURL: NSURL, ofType typeName: String) throws
  var fileType: String?
  @NSCopying var fileURL: NSURL?
  @NSCopying var fileModificationDate: NSDate?
  @available(OSX 10.8, *)
  var isDraft: Bool
  @available(OSX 10.7, *)
  func performActivity(withSynchronousWaiting waitSynchronously: Bool, using block: (() -> Void) -> Void)
  @available(OSX 10.7, *)
  func continueActivity(_ block: () -> Void)
  @available(OSX 10.7, *)
  func continueAsynchronousWorkOnMainThread(_ block: () -> Void)
  @available(OSX 10.7, *)
  func performSynchronousFileAccess(_ block: () -> Void)
  @available(OSX 10.7, *)
  func performAsynchronousFileAccess(_ block: (() -> Void) -> Void)
  @IBAction func revertDocumentToSaved(_ sender: AnyObject?)
  func revert(toContentsOf url: NSURL, ofType typeName: String) throws
  func read(from url: NSURL, ofType typeName: String) throws
  func read(from fileWrapper: NSFileWrapper, ofType typeName: String) throws
  func read(from data: NSData, ofType typeName: String) throws
  @available(OSX 10.7, *)
  var isEntireFileLoaded: Bool { get }
  func write(to url: NSURL, ofType typeName: String) throws
  @discardableResult
  func fileWrapper(ofType typeName: String) throws -> NSFileWrapper
  @discardableResult
  func data(ofType typeName: String) throws -> NSData
  @available(OSX 10.7, *)
  func unblockUserInteraction()
  @available(OSX 10.7, *)
  var autosavingIsImplicitlyCancellable: Bool { get }
  func writeSafely(to url: NSURL, ofType typeName: String, for saveOperation: NSSaveOperationType) throws
  func write(to url: NSURL, ofType typeName: String, for saveOperation: NSSaveOperationType, originalContentsURL absoluteOriginalContentsURL: NSURL?) throws
  @discardableResult
  func fileAttributesToWrite(to url: NSURL, ofType typeName: String, for saveOperation: NSSaveOperationType, originalContentsURL absoluteOriginalContentsURL: NSURL?) throws -> [String : AnyObject]
  var keepBackupFile: Bool { get }
  @available(OSX 10.8, *)
  @NSCopying var backupFileURL: NSURL? { get }
  @IBAction func save(_ sender: AnyObject?)
  @IBAction func saveAs(_ sender: AnyObject?)
  @IBAction func saveTo(_ sender: AnyObject?)
  func save(withDelegate delegate: AnyObject?, didSave didSaveSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  func runModalSavePanel(for saveOperation: NSSaveOperationType, delegate delegate: AnyObject?, didSave didSaveSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  var shouldRunSavePanelWithAccessoryView: Bool { get }
  @discardableResult
  func prepare(_ savePanel: NSSavePanel) -> Bool
  var fileNameExtensionWasHiddenInLastRunSavePanel: Bool { get }
  var fileTypeFromLastRunSavePanel: String? { get }
  func save(to url: NSURL, ofType typeName: String, for saveOperation: NSSaveOperationType, delegate delegate: AnyObject?, didSave didSaveSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @available(OSX 10.7, *)
  func save(to url: NSURL, ofType typeName: String, for saveOperation: NSSaveOperationType, completionHandler completionHandler: (NSError?) -> Void)
  @available(OSX 10.7, *)
  @discardableResult
  func canAsynchronouslyWrite(to url: NSURL, ofType typeName: String, for saveOperation: NSSaveOperationType) -> Bool
  @available(OSX 10.7, *)
  func checkAutosavingSafety() throws
  @available(OSX 10.7, *)
  func scheduleAutosaving()
  var hasUnautosavedChanges: Bool { get }
  func autosave(withDelegate delegate: AnyObject?, didAutosave didAutosaveSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @available(OSX 10.7, *)
  func autosave(withImplicitCancellability autosavingIsImplicitlyCancellable: Bool, completionHandler completionHandler: (NSError?) -> Void)
  @available(OSX 10.7, *)
  @discardableResult
  class func autosavesInPlace() -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  class func preservesVersions() -> Bool
  @available(OSX 10.8, *)
  @IBAction func browseDocumentVersions(_ sender: AnyObject?)
  @available(OSX 10.8, *)
  @discardableResult
  class func autosavesDrafts() -> Bool
  var autosavingFileType: String? { get }
  @NSCopying var autosavedContentsFileURL: NSURL?
  func canClose(withDelegate delegate: AnyObject, shouldClose shouldCloseSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  func close()
  @available(OSX 10.7, *)
  @IBAction func duplicate(_ sender: AnyObject?)
  @available(OSX 10.7, *)
  func duplicate(withDelegate delegate: AnyObject?, didDuplicate didDuplicateSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @available(OSX 10.7, *)
  @discardableResult
  func duplicate() throws -> NSDocument
  @available(OSX 10.8, *)
  @IBAction func renameDocument(_ sender: AnyObject?)
  @available(OSX 10.8, *)
  @IBAction func moveToUbiquityContainer(_ sender: AnyObject?)
  @available(OSX 10.8, *)
  @IBAction func move(_ sender: AnyObject?)
  @available(OSX 10.8, *)
  func move(completionHandler completionHandler: ((Bool) -> Void)? = nil)
  @available(OSX 10.8, *)
  func move(to url: NSURL, completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(OSX 10.8, *)
  @IBAction func lock(_ sender: AnyObject?)
  @available(OSX 10.8, *)
  @IBAction func unlock(_ sender: AnyObject?)
  @available(OSX 10.8, *)
  func lock(completionHandler completionHandler: ((Bool) -> Void)? = nil)
  @available(OSX 10.8, *)
  func lock(completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(OSX 10.8, *)
  func unlock(completionHandler completionHandler: ((Bool) -> Void)? = nil)
  @available(OSX 10.8, *)
  func unlock(completionHandler completionHandler: ((NSError?) -> Void)? = nil)
  @available(OSX 10.8, *)
  var isLocked: Bool { get }
  @IBAction func runPageLayout(_ sender: AnyObject?)
  func runModalPageLayout(with printInfo: NSPrintInfo, delegate delegate: AnyObject?, didRun didRunSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @discardableResult
  func prepare(_ pageLayout: NSPageLayout) -> Bool
  @discardableResult
  func shouldChangePrintInfo(_ newPrintInfo: NSPrintInfo) -> Bool
  @NSCopying var printInfo: NSPrintInfo
  @warn_unqualified_access
  @IBAction func print(_ sender: AnyObject?)
  func print(withSettings printSettings: [String : AnyObject], showPrintPanel showPrintPanel: Bool, delegate delegate: AnyObject?, didPrint didPrintSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @discardableResult
  func printOperation(withSettings printSettings: [String : AnyObject]) throws -> NSPrintOperation
  func runModalPrintOperation(_ printOperation: NSPrintOperation, delegate delegate: AnyObject?, didRun didRunSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @available(OSX 10.9, *)
  @IBAction func saveToPDF(_ sender: AnyObject?)
  @available(OSX 10.9, *)
  var pdfPrintOperation: NSPrintOperation { get }
  var isDocumentEdited: Bool { get }
  @available(OSX 10.7, *)
  var isInViewingMode: Bool { get }
  func updateChangeCount(_ change: NSDocumentChangeType)
  @available(OSX 10.7, *)
  @discardableResult
  func changeCountToken(for saveOperation: NSSaveOperationType) -> AnyObject
  @available(OSX 10.7, *)
  func updateChangeCount(withToken changeCountToken: AnyObject, for saveOperation: NSSaveOperationType)
  var undoManager: NSUndoManager?
  var hasUndoManager: Bool
  func presentError(_ error: NSError, modalFor window: NSWindow, delegate delegate: AnyObject?, didPresent didPresentSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @discardableResult
  func presentError(_ error: NSError) -> Bool
  @discardableResult
  func willPresentError(_ error: NSError) -> NSError
  @available(OSX 10.7, *)
  func willNotPresentError(_ error: NSError)
  func makeWindowControllers()
  var windowNibName: String? { get }
  func windowControllerWillLoadNib(_ windowController: NSWindowController)
  func windowControllerDidLoadNib(_ windowController: NSWindowController)
  func setWindow(_ window: NSWindow?)
  func addWindowController(_ windowController: NSWindowController)
  func removeWindowController(_ windowController: NSWindowController)
  func showWindows()
  var windowControllers: [NSWindowController] { get }
  func shouldCloseWindowController(_ windowController: NSWindowController, delegate delegate: AnyObject?, shouldClose shouldCloseSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @available(OSX 10.7, *)
  func setDisplayName(_ displayNameOrNil: String?)
  var displayName: String { get }
  @available(OSX 10.8, *)
  @discardableResult
  func defaultDraftName() -> String
  var windowForSheet: NSWindow? { get }
  @discardableResult
  class func readableTypes() -> [String]
  @discardableResult
  class func writableTypes() -> [String]
  @discardableResult
  class func isNativeType(_ type: String) -> Bool
  @discardableResult
  func writableTypes(for saveOperation: NSSaveOperationType) -> [String]
  @available(OSX 10.5, *)
  @discardableResult
  func fileNameExtension(forType typeName: String, saveOperation saveOperation: NSSaveOperationType) -> String?
  @available(OSX 10.8, *)
  @discardableResult
  class func usesUbiquitousStorage() -> Bool
}
struct __docFlags {
  var inClose: UInt32
  var hasUndoManager: UInt32
  var unused: UInt32
  var reconciledToFileName: UInt32
  var checkingDisplayName: UInt32
  var hasInvalidRestorableState: UInt32
  var hasEverHadInvalidRestorableState: UInt32
  var RESERVED: UInt32
  init()
  init(inClose inClose: UInt32, hasUndoManager hasUndoManager: UInt32, unused unused: UInt32, reconciledToFileName reconciledToFileName: UInt32, checkingDisplayName checkingDisplayName: UInt32, hasInvalidRestorableState hasInvalidRestorableState: UInt32, hasEverHadInvalidRestorableState hasEverHadInvalidRestorableState: UInt32, RESERVED RESERVED: UInt32)
}
extension NSDocument {
}
