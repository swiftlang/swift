
class IOBluetoothServiceBrowserController : NSWindowController {
  /*not inherited*/ init!(_ inOptions: IOBluetoothServiceBrowserControllerOptions)
  @discardableResult
  class func withServiceBrowserControllerRef(_ serviceBrowserControllerRef: IOBluetoothServiceBrowserControllerRef!) -> IOBluetoothServiceBrowserController!
  @discardableResult
  func getRef() -> Unmanaged<IOBluetoothServiceBrowserControllerRef>!
  func setOptions(_ inOptions: IOBluetoothServiceBrowserControllerOptions)
  @discardableResult
  func runModal() -> Int32
  @discardableResult
  func beginSheetModal(for sheetWindow: NSWindow!, modalDelegate modalDelegate: AnyObject!, didEnd didEndSelector: Selector!, contextInfo contextInfo: UnsafeMutablePointer<Void>!) -> IOReturn
  @discardableResult
  func getResults() -> [AnyObject]!
  @discardableResult
  func getOptions() -> IOBluetoothServiceBrowserControllerOptions
  func setSearchAttributes(_ searchAttributes: UnsafePointer<IOBluetoothDeviceSearchAttributes>!)
  @discardableResult
  func getSearchAttributes() -> UnsafePointer<IOBluetoothDeviceSearchAttributes>!
  func addAllowedUUID(_ allowedUUID: IOBluetoothSDPUUID!)
  func addAllowedUUIDArray(_ allowedUUIDArray: [AnyObject]!)
  func clearAllowedUUIDs()
  func setTitle(_ windowTitle: String!)
  @discardableResult
  func getTitle() -> String!
  func setDescriptionText(_ descriptionText: String!)
  @discardableResult
  func getDescriptionText() -> String!
  func setPrompt(_ prompt: String!)
  @discardableResult
  func getPrompt() -> String!
}
