
class IOBluetoothDeviceInquiry : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged AnyObject!
  init!(delegate delegate: AnyObject!)
  @discardableResult
  func start() -> IOReturn
  @discardableResult
  func stop() -> IOReturn
  var inquiryLength: UInt8
  var searchType: IOBluetoothDeviceSearchTypes
  var updateNewDeviceNames: Bool
  @discardableResult
  func foundDevices() -> [AnyObject]!
  func clearFoundDevices()
  func setSearchCriteria(_ inServiceClassMajor: BluetoothServiceClassMajor, majorDeviceClass inMajorDeviceClass: BluetoothDeviceClassMajor, minorDeviceClass inMinorDeviceClass: BluetoothDeviceClassMinor)
}
protocol IOBluetoothDeviceInquiryDelegate {
  optional func deviceInquiryStarted(_ sender: IOBluetoothDeviceInquiry!)
  optional func deviceInquiryDeviceFound(_ sender: IOBluetoothDeviceInquiry!, device device: IOBluetoothDevice!)
  optional func deviceInquiryUpdatingDeviceNamesStarted(_ sender: IOBluetoothDeviceInquiry!, devicesRemaining devicesRemaining: UInt32)
  optional func deviceInquiryDeviceNameUpdated(_ sender: IOBluetoothDeviceInquiry!, device device: IOBluetoothDevice!, devicesRemaining devicesRemaining: UInt32)
  optional func deviceInquiryComplete(_ sender: IOBluetoothDeviceInquiry!, error error: IOReturn, aborted aborted: Bool)
}
