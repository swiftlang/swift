
class NSWindowController : NSResponder, NSCoding, NSSeguePerforming {
  init(window window: NSWindow?)
  convenience init(windowNibName windowNibName: String)
  convenience init(windowNibName windowNibName: String, owner owner: AnyObject)
  convenience init(windowNibPath windowNibPath: String, owner owner: AnyObject)
  var windowNibName: String? { get }
  var windowNibPath: String? { get }
  unowned(unsafe) var owner: @sil_unmanaged AnyObject { get }
  var windowFrameAutosaveName: String?
  var shouldCascadeWindows: Bool
  unowned(unsafe) var document: @sil_unmanaged AnyObject?
  func setDocumentEdited(_ dirtyFlag: Bool)
  var shouldCloseDocument: Bool
  func synchronizeWindowTitleWithDocumentName()
  @discardableResult
  func windowTitle(forDocumentDisplayName displayName: String) -> String
  @available(OSX 10.10, *)
  var contentViewController: NSViewController?
  var window: NSWindow?
  var isWindowLoaded: Bool { get }
  func windowWillLoad()
  func windowDidLoad()
  func loadWindow()
  func close()
  @IBAction func showWindow(_ sender: AnyObject?)
}
struct __wcFlags {
  var shouldCloseDocument: UInt32
  var shouldCascade: UInt32
  var nibIsLoaded: UInt32
  var nibNameIsPath: UInt32
  var settingWindowsContentViewController: UInt32
  var didInitWithCoder: UInt32
  var nibIsMakingConnections: UInt32
  var sentWindowWillLoad: UInt32
  var RESERVED: UInt32
  init()
  init(shouldCloseDocument shouldCloseDocument: UInt32, shouldCascade shouldCascade: UInt32, nibIsLoaded nibIsLoaded: UInt32, nibNameIsPath nibNameIsPath: UInt32, settingWindowsContentViewController settingWindowsContentViewController: UInt32, didInitWithCoder didInitWithCoder: UInt32, nibIsMakingConnections nibIsMakingConnections: UInt32, sentWindowWillLoad sentWindowWillLoad: UInt32, RESERVED RESERVED: UInt32)
}
extension NSWindowController {
  @available(OSX 10.10, *)
  var storyboard: NSStoryboard? { get }
}
extension NSWindowController {
  @available(OSX 10.10, *)
  @IBAction func dismiss(_ sender: AnyObject?)
}
