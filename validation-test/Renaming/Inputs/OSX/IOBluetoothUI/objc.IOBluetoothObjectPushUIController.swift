
class IOBluetoothObjectPushUIController : NSWindowController {
  init!(objectPushWith inDevice: IOBluetoothDevice!, withFiles inFiles: [AnyObject]!, delegate inDelegate: AnyObject!)
  func runModal()
  func runPanel()
  @discardableResult
  func beginSheetModal(for sheetWindow: NSWindow!, modalDelegate modalDelegate: AnyObject!, didEnd didEndSelector: Selector!, contextInfo contextInfo: UnsafeMutablePointer<Void>!) -> IOReturn
  func stop()
  func setTitle(_ windowTitle: String!)
  @discardableResult
  func getTitle() -> String!
  func setIconImage(_ image: NSImage!)
  @discardableResult
  func getDevice() -> IOBluetoothDevice!
  @discardableResult
  func isTransferInProgress() -> Bool
}
