
class IOBluetoothPairingController : NSWindowController {
  @discardableResult
  func runModal() -> Int32
  @discardableResult
  func getResults() -> [AnyObject]!
  func setOptions(_ options: IOBluetoothServiceBrowserControllerOptions)
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
