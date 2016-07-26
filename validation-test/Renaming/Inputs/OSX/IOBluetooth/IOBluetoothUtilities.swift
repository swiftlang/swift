
@discardableResult
func IOBluetoothNSStringToDeviceAddress(_ inNameString: String!, _ outDeviceAddress: UnsafeMutablePointer<BluetoothDeviceAddress>!) -> IOReturn
@discardableResult
func IOBluetoothNSStringFromDeviceAddress(_ deviceAddress: UnsafePointer<BluetoothDeviceAddress>!) -> String!
@discardableResult
func IOBluetoothNSStringFromDeviceAddressColon(_ deviceAddress: UnsafePointer<BluetoothDeviceAddress>!) -> String!
@discardableResult
func IOBluetoothIsFileAppleDesignatedPIMData(_ inFileName: String!) -> Bool
@discardableResult
func IOBluetoothGetUniqueFileNameAndPath(_ inName: String!, _ inPath: String!) -> String!
@discardableResult
func IOBluetoothPackDataList(_ ioBuffer: UnsafeMutablePointer<Void>!, _ inFormat: UnsafePointer<Int8>!, _ inArgs: CVaListPointer) -> Int
@discardableResult
func IOBluetoothUnpackDataList(_ inBufferSize: Int, _ inBuffer: UnsafePointer<Void>!, _ inFormat: UnsafePointer<Int8>!, _ inArgs: CVaListPointer) -> Int
@discardableResult
func IOBluetoothNumberOfAvailableHIDDevices() -> Int
@discardableResult
func IOBluetoothNumberOfPointingHIDDevices() -> Int
@discardableResult
func IOBluetoothNumberOfKeyboardHIDDevices() -> Int
@discardableResult
func IOBluetoothNumberOfTabletHIDDevices() -> Int
@discardableResult
func IOBluetoothFindNumberOfRegistryEntriesOfClassName(_ deviceType: UnsafePointer<Int8>!) -> Int
