
enum UIDocumentChangeKind : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case done
  case undone
  case redone
  case cleared
}
enum UIDocumentSaveOperation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case forCreating
  case forOverwriting
}
struct UIDocumentState : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var closed: UIDocumentState { get }
  static var inConflict: UIDocumentState { get }
  static var savingError: UIDocumentState { get }
  static var editingDisabled: UIDocumentState { get }
  static var progressAvailable: UIDocumentState { get }
}
@available(iOS 5.0, *)
let UIDocumentStateChangedNotification: String
@available(iOS 5.0, *)
class UIDocument : NSObject, NSFilePresenter, NSProgressReporting {
  init(fileURL url: NSURL)
  var fileURL: NSURL { get }
  var localizedName: String { get }
  var fileType: String? { get }
  @NSCopying var fileModificationDate: NSDate?
  var documentState: UIDocumentState { get }
  func open(completionHandler completionHandler: ((Bool) -> Void)? = nil)
  func close(completionHandler completionHandler: ((Bool) -> Void)? = nil)
  func load(fromContents contents: AnyObject, ofType typeName: String?) throws
  @discardableResult
  func contents(forType typeName: String) throws -> AnyObject
  func disableEditing()
  func enableEditing()
  var undoManager: NSUndoManager!
  @discardableResult
  func hasUnsavedChanges() -> Bool
  func updateChangeCount(_ change: UIDocumentChangeKind)
  @discardableResult
  func changeCountToken(for saveOperation: UIDocumentSaveOperation) -> AnyObject
  func updateChangeCount(withToken changeCountToken: AnyObject, for saveOperation: UIDocumentSaveOperation)
  func save(to url: NSURL, for saveOperation: UIDocumentSaveOperation, completionHandler completionHandler: ((Bool) -> Void)? = nil)
  func autosave(completionHandler completionHandler: ((Bool) -> Void)? = nil)
  @discardableResult
  func savingFileType() -> String?
  @discardableResult
  func fileNameExtension(forType typeName: String?, saveOperation saveOperation: UIDocumentSaveOperation) -> String
  func writeContents(_ contents: AnyObject, andAttributes additionalFileAttributes: [NSObject : AnyObject]? = [:], safelyTo url: NSURL, for saveOperation: UIDocumentSaveOperation) throws
  func writeContents(_ contents: AnyObject, to url: NSURL, for saveOperation: UIDocumentSaveOperation, originalContentsURL originalContentsURL: NSURL?) throws
  @discardableResult
  func fileAttributesToWrite(to url: NSURL, for saveOperation: UIDocumentSaveOperation) throws -> [NSObject : AnyObject]
  func read(from url: NSURL) throws
  func performAsynchronousFileAccess(_ block: () -> Void)
  func handleError(_ error: NSError, userInteractionPermitted userInteractionPermitted: Bool)
  func finishedHandlingError(_ error: NSError, recovered recovered: Bool)
  func userInteractionNoLongerPermitted(forError error: NSError)
  func revert(toContentsOf url: NSURL, completionHandler completionHandler: ((Bool) -> Void)? = nil)
}
@available(iOS 8.0, *)
let NSUserActivityDocumentURLKey: String
extension UIDocument {
  @available(iOS 8.0, *)
  var userActivity: NSUserActivity?
  @available(iOS 8.0, *)
  func updateUserActivityState(_ userActivity: NSUserActivity)
  @available(iOS 8.0, *)
  func restoreUserActivityState(_ userActivity: NSUserActivity)
}
