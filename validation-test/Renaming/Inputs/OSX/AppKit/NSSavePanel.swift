
var NSFileHandlingPanelCancelButton: Int { get }
var NSFileHandlingPanelOKButton: Int { get }
struct __SPFlags {
  var saveMode: UInt32
  var isExpanded: UInt32
  var allowsOtherFileTypes: UInt32
  var canCreateDirectories: UInt32
  var canSelectedHiddenExtension: UInt32
  var reserved2: UInt32
  var delegate_shouldShowFilename: UInt32
  var delegate_compareFilename: UInt32
  var delegate_shouldEnableURL: UInt32
  var delegate_validateURL: UInt32
  var delegate_didChangeToDirectoryURL: UInt32
  var changingFrameSize: UInt32
  var movingAccessoryView: UInt32
  var userAccessoryViewFrameChange: UInt32
  var canChooseDirectories: UInt32
  var canChooseFiles: UInt32
  var delegate_selectionDidChange: UInt32
  var delegate_didChangeToDirectory: UInt32
  var calledWindowOrderedIn: UInt32
  var appCentric: UInt32
  var bottomControlsDisabled: UInt32
  var okButtonDisabled: UInt32
  var accessoryViewDisclosed: UInt32
  var delegate_isValidFilename: UInt32
  var delegate_userEnteredFilename: UInt32
  var delegate_panel_willExpand: UInt32
  var canResolveUbiquitousConflicts: UInt32
  var reserved: UInt32
  init()
  init(saveMode saveMode: UInt32, isExpanded isExpanded: UInt32, allowsOtherFileTypes allowsOtherFileTypes: UInt32, canCreateDirectories canCreateDirectories: UInt32, canSelectedHiddenExtension canSelectedHiddenExtension: UInt32, reserved2 reserved2: UInt32, delegate_shouldShowFilename delegate_shouldShowFilename: UInt32, delegate_compareFilename delegate_compareFilename: UInt32, delegate_shouldEnableURL delegate_shouldEnableURL: UInt32, delegate_validateURL delegate_validateURL: UInt32, delegate_didChangeToDirectoryURL delegate_didChangeToDirectoryURL: UInt32, changingFrameSize changingFrameSize: UInt32, movingAccessoryView movingAccessoryView: UInt32, userAccessoryViewFrameChange userAccessoryViewFrameChange: UInt32, canChooseDirectories canChooseDirectories: UInt32, canChooseFiles canChooseFiles: UInt32, delegate_selectionDidChange delegate_selectionDidChange: UInt32, delegate_didChangeToDirectory delegate_didChangeToDirectory: UInt32, calledWindowOrderedIn calledWindowOrderedIn: UInt32, appCentric appCentric: UInt32, bottomControlsDisabled bottomControlsDisabled: UInt32, okButtonDisabled okButtonDisabled: UInt32, accessoryViewDisclosed accessoryViewDisclosed: UInt32, delegate_isValidFilename delegate_isValidFilename: UInt32, delegate_userEnteredFilename delegate_userEnteredFilename: UInt32, delegate_panel_willExpand delegate_panel_willExpand: UInt32, canResolveUbiquitousConflicts canResolveUbiquitousConflicts: UInt32, reserved reserved: UInt32)
}
typealias _SPFlags = __SPFlags
class NSSavePanel : NSPanel {
  @NSCopying var url: NSURL? { get }
  @available(OSX 10.6, *)
  @NSCopying var directoryURL: NSURL?
  var allowedFileTypes: [String]?
  var allowsOtherFileTypes: Bool
  var accessoryView: NSView?
  var isExpanded: Bool { get }
  var canCreateDirectories: Bool
  var canSelectHiddenExtension: Bool
  var isExtensionHidden: Bool
  var treatsFilePackagesAsDirectories: Bool
  var prompt: String!
  var nameFieldLabel: String!
  @available(OSX 10.6, *)
  var nameFieldStringValue: String
  var message: String!
  func validateVisibleColumns()
  var showsHiddenFiles: Bool
  @available(OSX 10.9, *)
  var showsTagField: Bool
  @available(OSX 10.9, *)
  var tagNames: [String]?
  @IBAction func ok(_ sender: AnyObject?)
  @IBAction func cancel(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  func beginSheetModal(for window: NSWindow, completionHandler handler: (Int) -> Void)
  @available(OSX 10.6, *)
  func begin(completionHandler handler: (Int) -> Void)
  @discardableResult
  func runModal() -> Int
}
protocol NSOpenSavePanelDelegate : NSObjectProtocol {
  @available(OSX 10.6, *)
  @discardableResult
  optional func panel(_ sender: AnyObject, shouldEnable url: NSURL) -> Bool
  @available(OSX 10.6, *)
  optional func panel(_ sender: AnyObject, validate url: NSURL) throws
  @available(OSX 10.6, *)
  optional func panel(_ sender: AnyObject, didChangeToDirectoryURL url: NSURL?)
  @discardableResult
  optional func panel(_ sender: AnyObject, userEnteredFilename filename: String, confirmed okFlag: Bool) -> String?
  optional func panel(_ sender: AnyObject, willExpand expanding: Bool)
  optional func panelSelectionDidChange(_ sender: AnyObject?)
}
extension NSObject {
}
extension NSSavePanel {
}
