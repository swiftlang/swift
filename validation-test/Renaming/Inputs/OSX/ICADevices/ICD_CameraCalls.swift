
var kICD_FileData: Int { get }
var kICD_MetaData: Int { get }
var kICD_ThumbnailData: Int { get }
var kICD_ThumbnailDataFormatJPEG: Int { get }
var kICD_ThumbnailDataFormatTIFF: Int { get }
var kICD_ThumbnailDataFormatPNG: Int { get }
var hasChildrenMask: Int { get }
var hasThumbnailMask: Int { get }
var fileLockedMask: Int { get }
var rawImageFormatMask: Int { get }
var fileInTempCacheMask: Int { get }
var largeFileSizeMask: Int { get }
var addedAfterCCCMask: Int { get }
struct ObjectInfo {
  var icaObject: ICAObject
  var reserved: UInt
  var icaObjectInfo: ICAObjectInfo
  var uniqueID: UInt32
  var thumbnailSize: UInt32
  var dataSize: UInt32
  var dataWidth: UInt32
  var dataHeight: UInt32
  var name: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  var creationDate: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  var flags: UInt32
  var privateData: Ptr!
  var uniqueIDFireWire: UInt64
  var tag: UInt32
  var dataSize64: UInt64
  init()
  init(icaObject icaObject: ICAObject, reserved reserved: UInt, icaObjectInfo icaObjectInfo: ICAObjectInfo, uniqueID uniqueID: UInt32, thumbnailSize thumbnailSize: UInt32, dataSize dataSize: UInt32, dataWidth dataWidth: UInt32, dataHeight dataHeight: UInt32, name name: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8), creationDate creationDate: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8), flags flags: UInt32, privateData privateData: Ptr!, uniqueIDFireWire uniqueIDFireWire: UInt64, tag tag: UInt32, dataSize64 dataSize64: UInt64)
}
struct ICD_ObjectSendMessagePB {
  var header: ICDHeader
  var object: ICAObject
  var objectInfo: ICAObjectInfo
  var connectionID: ICAConnectionID
  var message: ICAMessage
  var totalDataSize: UInt32
  var result: UInt32
  init()
  init(header header: ICDHeader, object object: ICAObject, objectInfo objectInfo: ICAObjectInfo, connectionID connectionID: ICAConnectionID, message message: ICAMessage, totalDataSize totalDataSize: UInt32, result result: UInt32)
}
typealias __ICD_OpenUSBDevice = @convention(c) (UInt32, UnsafeMutablePointer<ObjectInfo>!) -> ICAError
typealias __ICD_OpenUSBDeviceWithIORegPath = @convention(c) (UInt32, UnsafeMutablePointer<Int8>!, UnsafeMutablePointer<ObjectInfo>!) -> ICAError
typealias __ICD_OpenFireWireDevice = @convention(c) (UInt64, UnsafeMutablePointer<ObjectInfo>!) -> ICAError
typealias __ICD_OpenFireWireDeviceWithIORegPath = @convention(c) (UInt64, UnsafeMutablePointer<Int8>!, UnsafeMutablePointer<ObjectInfo>!) -> ICAError
typealias __ICD_OpenBluetoothDevice = @convention(c) (CFDictionary!, UnsafeMutablePointer<ObjectInfo>!) -> ICAError
typealias __ICD_OpenTCPIPDevice = @convention(c) (CFDictionary!, UnsafeMutablePointer<ObjectInfo>!) -> ICAError
typealias __ICD_OpenMassStorageDevice = @convention(c) (CFString!, DASession!, UnsafeMutablePointer<ObjectInfo>!) -> ICAError
typealias __ICD_CloseDevice = @convention(c) (UnsafeMutablePointer<ObjectInfo>!) -> ICAError
typealias __ICD_PeriodicTask = @convention(c) (UnsafeMutablePointer<ObjectInfo>!) -> ICAError
typealias __ICD_GetObjectInfo = @convention(c) (UnsafePointer<ObjectInfo>!, UInt32, UnsafeMutablePointer<ObjectInfo>!) -> ICAError
typealias __ICD_Cleanup = @convention(c) (UnsafeMutablePointer<ObjectInfo>!) -> ICAError
typealias __ICD_GetPropertyData = @convention(c) (UnsafePointer<ObjectInfo>!, UnsafeMutablePointer<Void>!) -> ICAError
typealias __ICD_SetPropertyData = @convention(c) (UnsafePointer<ObjectInfo>!, UnsafePointer<Void>!) -> ICAError
typealias __ICD_ReadFileData = @convention(c) (UnsafePointer<ObjectInfo>!, UInt32, Ptr!, UInt32, UnsafeMutablePointer<UInt32>!) -> ICAError
typealias __ICD_WriteFileData = @convention(c) (UnsafePointer<ObjectInfo>!, UnsafePointer<Int8>!, UInt32, Ptr!, UInt32, UnsafeMutablePointer<UInt32>!) -> ICAError
typealias __ICD_SendMessage = @convention(c) (UnsafePointer<ObjectInfo>!, UnsafeMutablePointer<ICD_ObjectSendMessagePB>!, ICDCompletion!) -> ICAError
typealias __ICD_AddPropertiesToCFDictionary = @convention(c) (UnsafeMutablePointer<ObjectInfo>!, CFMutableDictionary!) -> ICAError
typealias __ICD_WriteDataToFile = @convention(c) (UnsafePointer<ObjectInfo>!, UnsafeMutablePointer<FILE>!, UInt32, UnsafeMutablePointer<Int>!) -> ICAError
typealias __ICD_WriteDataToFileDescriptor = @convention(c) (UnsafePointer<ObjectInfo>!, Int32, UInt32, UnsafeMutablePointer<Int>!) -> ICAError
typealias __ICD_WriteDataToFileDescriptor64 = @convention(c) (UnsafePointer<ObjectInfo>!, Int32) -> ICAError
@discardableResult
func ICD_main(_ argc: Int32, _ argv: UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32
@discardableResult
func ICDGetStandardPropertyData(_ objectInfo: UnsafePointer<ObjectInfo>!, _ pb: UnsafeMutablePointer<Void>!) -> ICAError
@discardableResult
func ICDNewObjectInfoCreated(_ parentInfo: UnsafePointer<ObjectInfo>!, _ index: UInt32, _ newICAObject: UnsafeMutablePointer<ICAObject>!) -> ICAError
typealias ICDNewObjectCreatedCompletion = @convention(c) (UnsafePointer<ObjectInfo>!) -> Void
@discardableResult
func ICDNewObjectCreated(_ parentInfo: UnsafePointer<ObjectInfo>!, _ objectInfo: UnsafePointer<ObjectInfo>!, _ completion: ICDNewObjectCreatedCompletion!) -> ICAError
@discardableResult
func ICDCopyDeviceInfoDictionary(_ deviceName: UnsafePointer<Int8>!, _ theDict: UnsafeMutablePointer<Unmanaged<CFDictionary>?>!) -> ICAError
@discardableResult
func ICDCreateEventDataCookie(_ object: ICAObject, _ cookie: UnsafeMutablePointer<ICAEventDataCookie>!) -> ICAError
@discardableResult
func ICDConnectUSBDevice(_ locationID: UInt32) -> ICAError
@discardableResult
func ICDConnectUSBDeviceWithIORegPath(_ locationID: UInt32, _ ioregPath: UnsafeMutablePointer<Int8>!) -> ICAError
@discardableResult
func ICDDisconnectUSBDevice(_ locationID: UInt32) -> ICAError
@discardableResult
func ICDDisconnectUSBDeviceWithIORegPath(_ locationID: UInt32, _ ioregPath: UnsafeMutablePointer<Int8>!) -> ICAError
@discardableResult
func ICDConnectFWDevice(_ guid: UInt64) -> ICAError
@discardableResult
func ICDConnectFWDeviceWithIORegPath(_ guid: UInt64, _ ioregPath: UnsafeMutablePointer<Int8>!) -> ICAError
@discardableResult
func ICDDisconnectFWDevice(_ guid: UInt64) -> ICAError
@discardableResult
func ICDDisconnectFWDeviceWithIORegPath(_ guid: UInt64, _ ioregPath: UnsafeMutablePointer<Int8>!) -> ICAError
@discardableResult
func ICDConnectBluetoothDevice(_ params: CFDictionary!) -> ICAError
@discardableResult
func ICDDisconnectBluetoothDevice(_ params: CFDictionary!) -> ICAError
@discardableResult
func ICDConnectTCPIPDevice(_ params: CFDictionary!) -> ICAError
@discardableResult
func ICDDisconnectTCPIPDevice(_ params: CFDictionary!) -> ICAError
struct ICD_callback_functions {
  var f_ICD_OpenUSBDevice: __ICD_OpenUSBDevice!
  var f_ICD_CloseDevice: __ICD_CloseDevice!
  var f_ICD_PeriodicTask: __ICD_PeriodicTask!
  var f_ICD_GetObjectInfo: __ICD_GetObjectInfo!
  var f_ICD_Cleanup: __ICD_Cleanup!
  var f_ICD_GetPropertyData: __ICD_GetPropertyData!
  var f_ICD_SetPropertyData: __ICD_SetPropertyData!
  var f_ICD_ReadFileData: __ICD_ReadFileData!
  var f_ICD_WriteFileData: __ICD_WriteFileData!
  var f_ICD_SendMessage: __ICD_SendMessage!
  var f_ICD_AddPropertiesToCFDictionary: __ICD_AddPropertiesToCFDictionary!
  var f_ICD_OpenFireWireDevice: __ICD_OpenFireWireDevice!
  var f_ICD_OpenUSBDeviceWithIORegPath: __ICD_OpenUSBDeviceWithIORegPath!
  var f_ICD_OpenFireWireDeviceWithIORegPath: __ICD_OpenFireWireDeviceWithIORegPath!
  var f_ICD_OpenBluetoothDevice: __ICD_OpenBluetoothDevice!
  var f_ICD_OpenTCPIPDevice: __ICD_OpenTCPIPDevice!
  var f_ICD_WriteDataToFile: __ICD_WriteDataToFile!
  var f_ICD_OpenMassStorageDevice: __ICD_OpenMassStorageDevice!
  var f_ICD_WriteDataToFileDescriptor: __ICD_WriteDataToFileDescriptor!
  var f_ICD_WriteDataToFileDescriptor64: __ICD_WriteDataToFileDescriptor64!
  init()
  init(f_ICD_OpenUSBDevice f_ICD_OpenUSBDevice: __ICD_OpenUSBDevice!, f_ICD_CloseDevice f_ICD_CloseDevice: __ICD_CloseDevice!, f_ICD_PeriodicTask f_ICD_PeriodicTask: __ICD_PeriodicTask!, f_ICD_GetObjectInfo f_ICD_GetObjectInfo: __ICD_GetObjectInfo!, f_ICD_Cleanup f_ICD_Cleanup: __ICD_Cleanup!, f_ICD_GetPropertyData f_ICD_GetPropertyData: __ICD_GetPropertyData!, f_ICD_SetPropertyData f_ICD_SetPropertyData: __ICD_SetPropertyData!, f_ICD_ReadFileData f_ICD_ReadFileData: __ICD_ReadFileData!, f_ICD_WriteFileData f_ICD_WriteFileData: __ICD_WriteFileData!, f_ICD_SendMessage f_ICD_SendMessage: __ICD_SendMessage!, f_ICD_AddPropertiesToCFDictionary f_ICD_AddPropertiesToCFDictionary: __ICD_AddPropertiesToCFDictionary!, f_ICD_OpenFireWireDevice f_ICD_OpenFireWireDevice: __ICD_OpenFireWireDevice!, f_ICD_OpenUSBDeviceWithIORegPath f_ICD_OpenUSBDeviceWithIORegPath: __ICD_OpenUSBDeviceWithIORegPath!, f_ICD_OpenFireWireDeviceWithIORegPath f_ICD_OpenFireWireDeviceWithIORegPath: __ICD_OpenFireWireDeviceWithIORegPath!, f_ICD_OpenBluetoothDevice f_ICD_OpenBluetoothDevice: __ICD_OpenBluetoothDevice!, f_ICD_OpenTCPIPDevice f_ICD_OpenTCPIPDevice: __ICD_OpenTCPIPDevice!, f_ICD_WriteDataToFile f_ICD_WriteDataToFile: __ICD_WriteDataToFile!, f_ICD_OpenMassStorageDevice f_ICD_OpenMassStorageDevice: __ICD_OpenMassStorageDevice!, f_ICD_WriteDataToFileDescriptor f_ICD_WriteDataToFileDescriptor: __ICD_WriteDataToFileDescriptor!, f_ICD_WriteDataToFileDescriptor64 f_ICD_WriteDataToFileDescriptor64: __ICD_WriteDataToFileDescriptor64!)
}
var gICDCallbackFunctions: ICD_callback_functions
