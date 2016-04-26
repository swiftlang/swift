
var kICS_FileData: Int { get }
var kICS_MetaData: Int { get }
var kICS_ThumbnailData: Int { get }
var kICS_ThumbnailDataFormatJPEG: Int { get }
var kICS_ThumbnailDataFormatTIFF: Int { get }
var kICS_ThumbnailDataFormatPNG: Int { get }
struct ScannerObjectInfo {
  var icaObject: ICAObject
  var reserved: UInt
  var icaObjectInfo: ICAObjectInfo
  var uniqueID: UInt32
  var uniqueIDFireWire: UInt64
  var thumbnailSize: UInt32
  var dataSize: UInt32
  var dataWidth: UInt32
  var dataHeight: UInt32
  var name: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  var creationDate: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  var flags: UInt32
  var privateData: Ptr!
  var tag: UInt32
  init()
  init(icaObject icaObject: ICAObject, reserved reserved: UInt, icaObjectInfo icaObjectInfo: ICAObjectInfo, uniqueID uniqueID: UInt32, uniqueIDFireWire uniqueIDFireWire: UInt64, thumbnailSize thumbnailSize: UInt32, dataSize dataSize: UInt32, dataWidth dataWidth: UInt32, dataHeight dataHeight: UInt32, name name: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8), creationDate creationDate: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8), flags flags: UInt32, privateData privateData: Ptr!, tag tag: UInt32)
}
struct ICD_ScannerObjectSendMessagePB {
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
struct ICD_ScannerOpenSessionPB {
  var header: ICDHeader
  var object: ICAObject
  var objectInfo: ICAObjectInfo
  var connectionID: ICAConnectionID
  var sessionID: ICAScannerSessionID
  init()
  init(header header: ICDHeader, object object: ICAObject, objectInfo objectInfo: ICAObjectInfo, connectionID connectionID: ICAConnectionID, sessionID sessionID: ICAScannerSessionID)
}
struct ICD_ScannerCloseSessionPB {
  var header: ICDHeader
  var object: ICAObject
  var objectInfo: ICAObjectInfo
  var connectionID: ICAConnectionID
  var sessionID: ICAScannerSessionID
  init()
  init(header header: ICDHeader, object object: ICAObject, objectInfo objectInfo: ICAObjectInfo, connectionID connectionID: ICAConnectionID, sessionID sessionID: ICAScannerSessionID)
}
struct ICD_ScannerInitializePB {
  var header: ICDHeader
  var object: ICAObject
  var objectInfo: ICAObjectInfo
  var connectionID: ICAConnectionID
  var sessionID: ICAScannerSessionID
  init()
  init(header header: ICDHeader, object object: ICAObject, objectInfo objectInfo: ICAObjectInfo, connectionID connectionID: ICAConnectionID, sessionID sessionID: ICAScannerSessionID)
}
struct ICD_ScannerGetParametersPB {
  var header: ICDHeader
  var object: ICAObject
  var objectInfo: ICAObjectInfo
  var connectionID: ICAConnectionID
  var sessionID: ICAScannerSessionID
  var theDict: Unmanaged<CFMutableDictionary>!
  init()
  init(header header: ICDHeader, object object: ICAObject, objectInfo objectInfo: ICAObjectInfo, connectionID connectionID: ICAConnectionID, sessionID sessionID: ICAScannerSessionID, theDict theDict: Unmanaged<CFMutableDictionary>!)
}
struct ICD_ScannerSetParametersPB {
  var header: ICDHeader
  var object: ICAObject
  var objectInfo: ICAObjectInfo
  var connectionID: ICAConnectionID
  var sessionID: ICAScannerSessionID
  var theDict: Unmanaged<CFMutableDictionary>!
  init()
  init(header header: ICDHeader, object object: ICAObject, objectInfo objectInfo: ICAObjectInfo, connectionID connectionID: ICAConnectionID, sessionID sessionID: ICAScannerSessionID, theDict theDict: Unmanaged<CFMutableDictionary>!)
}
struct ICD_ScannerStatusPB {
  var header: ICDHeader
  var object: ICAObject
  var objectInfo: ICAObjectInfo
  var connectionID: ICAConnectionID
  var sessionID: ICAScannerSessionID
  var status: UInt32
  init()
  init(header header: ICDHeader, object object: ICAObject, objectInfo objectInfo: ICAObjectInfo, connectionID connectionID: ICAConnectionID, sessionID sessionID: ICAScannerSessionID, status status: UInt32)
}
struct ICD_ScannerStartPB {
  var header: ICDHeader
  var object: ICAObject
  var objectInfo: ICAObjectInfo
  var connectionID: ICAConnectionID
  var sessionID: ICAScannerSessionID
  init()
  init(header header: ICDHeader, object object: ICAObject, objectInfo objectInfo: ICAObjectInfo, connectionID connectionID: ICAConnectionID, sessionID sessionID: ICAScannerSessionID)
}
typealias __ICD_ScannerOpenUSBDevice = @convention(c) (UInt32, UnsafeMutablePointer<ScannerObjectInfo>!) -> ICAError
typealias __ICD_ScannerOpenUSBDeviceWithIORegPath = @convention(c) (UInt32, UnsafeMutablePointer<Int8>!, UnsafeMutablePointer<ScannerObjectInfo>!) -> ICAError
typealias __ICD_ScannerOpenFireWireDevice = @convention(c) (UInt64, UnsafeMutablePointer<ScannerObjectInfo>!) -> ICAError
typealias __ICD_ScannerOpenFireWireDeviceWithIORegPath = @convention(c) (UInt64, UnsafeMutablePointer<Int8>!, UnsafeMutablePointer<ScannerObjectInfo>!) -> ICAError
typealias __ICD_ScannerOpenBluetoothDevice = @convention(c) (CFDictionary!, UnsafeMutablePointer<ScannerObjectInfo>!) -> ICAError
typealias __ICD_ScannerOpenTCPIPDevice = @convention(c) (CFDictionary!, UnsafeMutablePointer<ScannerObjectInfo>!) -> ICAError
typealias __ICD_ScannerOpenMassStorageDevice = @convention(c) (CFString!, DASession!, UnsafeMutablePointer<ScannerObjectInfo>!) -> ICAError
typealias __ICD_ScannerCloseDevice = @convention(c) (UnsafeMutablePointer<ScannerObjectInfo>!) -> ICAError
typealias __ICD_ScannerPeriodicTask = @convention(c) (UnsafeMutablePointer<ScannerObjectInfo>!) -> ICAError
typealias __ICD_ScannerGetObjectInfo = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UInt32, UnsafeMutablePointer<ScannerObjectInfo>!) -> ICAError
typealias __ICD_ScannerCleanup = @convention(c) (UnsafeMutablePointer<ScannerObjectInfo>!) -> ICAError
typealias __ICD_ScannerGetPropertyData = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafeMutablePointer<Void>!) -> ICAError
typealias __ICD_ScannerSetPropertyData = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafePointer<Void>!) -> ICAError
typealias __ICD_ScannerReadFileData = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UInt32, Ptr!, UInt32, UnsafeMutablePointer<UInt32>!) -> ICAError
typealias __ICD_ScannerWriteFileData = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafePointer<Int8>!, UInt32, Ptr!, UInt32, UnsafeMutablePointer<UInt32>!) -> ICAError
typealias __ICD_ScannerSendMessage = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafeMutablePointer<ICD_ScannerObjectSendMessagePB>!, ICDCompletion!) -> ICAError
typealias __ICD_ScannerAddPropertiesToCFDictionary = @convention(c) (UnsafeMutablePointer<ScannerObjectInfo>!, CFMutableDictionary!) -> ICAError
typealias __ICD_ScannerOpenSession = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafeMutablePointer<ICD_ScannerOpenSessionPB>!) -> ICAError
typealias __ICD_ScannerCloseSession = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafeMutablePointer<ICD_ScannerCloseSessionPB>!) -> ICAError
typealias __ICD_ScannerInitialize = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafeMutablePointer<ICD_ScannerInitializePB>!) -> ICAError
typealias __ICD_ScannerGetParameters = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafeMutablePointer<ICD_ScannerGetParametersPB>!) -> ICAError
typealias __ICD_ScannerSetParameters = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafeMutablePointer<ICD_ScannerSetParametersPB>!) -> ICAError
typealias __ICD_ScannerStatus = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafeMutablePointer<ICD_ScannerStatusPB>!) -> ICAError
typealias __ICD_ScannerStart = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafeMutablePointer<ICD_ScannerStartPB>!) -> ICAError
typealias __ICD_ScannerWriteDataToFile = @convention(c) (UnsafePointer<ScannerObjectInfo>!, UnsafeMutablePointer<FILE>!, UInt32, UnsafeMutablePointer<Int>!) -> ICAError
typealias __ICD_ScannerWriteDataToFileDescriptor = @convention(c) (UnsafePointer<ScannerObjectInfo>!, Int32, UInt32, UnsafeMutablePointer<Int>!) -> ICAError
typealias __ICD_ScannerWriteDataToFileDescriptor64 = @convention(c) (UnsafePointer<ScannerObjectInfo>!, Int32) -> ICAError
struct ICD_Scannerscanner_callback_functions {
  var f_ICD_ScannerOpenUSBDevice: __ICD_ScannerOpenUSBDevice!
  var f_ICD_ScannerOpenUSBDeviceWithIORegPath: __ICD_ScannerOpenUSBDeviceWithIORegPath!
  var f_ICD_ScannerCloseDevice: __ICD_ScannerCloseDevice!
  var f_ICD_ScannerPeriodicTask: __ICD_ScannerPeriodicTask!
  var f_ICD_ScannerGetObjectInfo: __ICD_ScannerGetObjectInfo!
  var f_ICD_ScannerCleanup: __ICD_ScannerCleanup!
  var f_ICD_ScannerGetPropertyData: __ICD_ScannerGetPropertyData!
  var f_ICD_ScannerSetPropertyData: __ICD_ScannerSetPropertyData!
  var f_ICD_ScannerReadFileData: __ICD_ScannerReadFileData!
  var f_ICD_ScannerWriteFileData: __ICD_ScannerWriteFileData!
  var f_ICD_ScannerSendMessage: __ICD_ScannerSendMessage!
  var f_ICD_ScannerAddPropertiesToCFDictionary: __ICD_ScannerAddPropertiesToCFDictionary!
  var f_ICD_ScannerOpenFireWireDevice: __ICD_ScannerOpenFireWireDevice!
  var f_ICD_ScannerOpenFireWireDeviceWithIORegPath: __ICD_ScannerOpenFireWireDeviceWithIORegPath!
  var f_ICD_ScannerOpenSession: __ICD_ScannerOpenSession!
  var f_ICD_ScannerCloseSession: __ICD_ScannerCloseSession!
  var f_ICD_ScannerInitialize: __ICD_ScannerInitialize!
  var f_ICD_ScannerGetParameters: __ICD_ScannerGetParameters!
  var f_ICD_ScannerSetParameters: __ICD_ScannerSetParameters!
  var f_ICD_ScannerStatus: __ICD_ScannerStatus!
  var f_ICD_ScannerStart: __ICD_ScannerStart!
  var f_ICD_ScannerOpenBluetoothDevice: __ICD_ScannerOpenBluetoothDevice!
  var f_ICD_ScannerOpenTCPIPDevice: __ICD_ScannerOpenTCPIPDevice!
  var f_ICD_ScannerWriteDataToFile: __ICD_ScannerWriteDataToFile!
  var f_ICD_ScannerOpenMassStorageDevice: __ICD_ScannerOpenMassStorageDevice!
  var f_ICD_ScannerWriteDataToFileDescriptor: __ICD_ScannerWriteDataToFileDescriptor!
  var f_ICD_ScannerWriteDataToFileDescriptor64: __ICD_ScannerWriteDataToFileDescriptor64!
  init()
  init(f_ICD_ScannerOpenUSBDevice f_ICD_ScannerOpenUSBDevice: __ICD_ScannerOpenUSBDevice!, f_ICD_ScannerOpenUSBDeviceWithIORegPath f_ICD_ScannerOpenUSBDeviceWithIORegPath: __ICD_ScannerOpenUSBDeviceWithIORegPath!, f_ICD_ScannerCloseDevice f_ICD_ScannerCloseDevice: __ICD_ScannerCloseDevice!, f_ICD_ScannerPeriodicTask f_ICD_ScannerPeriodicTask: __ICD_ScannerPeriodicTask!, f_ICD_ScannerGetObjectInfo f_ICD_ScannerGetObjectInfo: __ICD_ScannerGetObjectInfo!, f_ICD_ScannerCleanup f_ICD_ScannerCleanup: __ICD_ScannerCleanup!, f_ICD_ScannerGetPropertyData f_ICD_ScannerGetPropertyData: __ICD_ScannerGetPropertyData!, f_ICD_ScannerSetPropertyData f_ICD_ScannerSetPropertyData: __ICD_ScannerSetPropertyData!, f_ICD_ScannerReadFileData f_ICD_ScannerReadFileData: __ICD_ScannerReadFileData!, f_ICD_ScannerWriteFileData f_ICD_ScannerWriteFileData: __ICD_ScannerWriteFileData!, f_ICD_ScannerSendMessage f_ICD_ScannerSendMessage: __ICD_ScannerSendMessage!, f_ICD_ScannerAddPropertiesToCFDictionary f_ICD_ScannerAddPropertiesToCFDictionary: __ICD_ScannerAddPropertiesToCFDictionary!, f_ICD_ScannerOpenFireWireDevice f_ICD_ScannerOpenFireWireDevice: __ICD_ScannerOpenFireWireDevice!, f_ICD_ScannerOpenFireWireDeviceWithIORegPath f_ICD_ScannerOpenFireWireDeviceWithIORegPath: __ICD_ScannerOpenFireWireDeviceWithIORegPath!, f_ICD_ScannerOpenSession f_ICD_ScannerOpenSession: __ICD_ScannerOpenSession!, f_ICD_ScannerCloseSession f_ICD_ScannerCloseSession: __ICD_ScannerCloseSession!, f_ICD_ScannerInitialize f_ICD_ScannerInitialize: __ICD_ScannerInitialize!, f_ICD_ScannerGetParameters f_ICD_ScannerGetParameters: __ICD_ScannerGetParameters!, f_ICD_ScannerSetParameters f_ICD_ScannerSetParameters: __ICD_ScannerSetParameters!, f_ICD_ScannerStatus f_ICD_ScannerStatus: __ICD_ScannerStatus!, f_ICD_ScannerStart f_ICD_ScannerStart: __ICD_ScannerStart!, f_ICD_ScannerOpenBluetoothDevice f_ICD_ScannerOpenBluetoothDevice: __ICD_ScannerOpenBluetoothDevice!, f_ICD_ScannerOpenTCPIPDevice f_ICD_ScannerOpenTCPIPDevice: __ICD_ScannerOpenTCPIPDevice!, f_ICD_ScannerWriteDataToFile f_ICD_ScannerWriteDataToFile: __ICD_ScannerWriteDataToFile!, f_ICD_ScannerOpenMassStorageDevice f_ICD_ScannerOpenMassStorageDevice: __ICD_ScannerOpenMassStorageDevice!, f_ICD_ScannerWriteDataToFileDescriptor f_ICD_ScannerWriteDataToFileDescriptor: __ICD_ScannerWriteDataToFileDescriptor!, f_ICD_ScannerWriteDataToFileDescriptor64 f_ICD_ScannerWriteDataToFileDescriptor64: __ICD_ScannerWriteDataToFileDescriptor64!)
}
typealias ICD_scanner_callback_functions = ICD_Scannerscanner_callback_functions
var gICDScannerCallbackFunctions: ICD_scanner_callback_functions
@discardableResult
func ICD_ScannerMain(_ argc: Int32, _ argv: UnsafeMutablePointer<UnsafePointer<Int8>?>!) -> Int32
@discardableResult
func ICDScannerGetStandardPropertyData(_ objectInfo: UnsafePointer<ScannerObjectInfo>!, _ pb: UnsafeMutablePointer<Void>!) -> ICAError
@discardableResult
func ICDScannerNewObjectInfoCreated(_ parentInfo: UnsafePointer<ScannerObjectInfo>!, _ index: UInt32, _ newICAObject: UnsafeMutablePointer<ICAObject>!) -> ICAError
@discardableResult
func ICDScannerCopyDeviceInfoDictionary(_ deviceName: UnsafePointer<Int8>!, _ theDict: UnsafeMutablePointer<Unmanaged<CFDictionary>?>!) -> ICAError
@discardableResult
func ICDScannerCreateEventDataCookie(_ object: ICAObject, _ cookie: UnsafeMutablePointer<ICAEventDataCookie>!) -> ICAError
@discardableResult
func ICDScannerConnectUSBDevice(_ locationID: UInt32) -> ICAError
@discardableResult
func ICDScannerConnectUSBDeviceWithIORegPath(_ locationID: UInt32, _ ioregPath: UnsafeMutablePointer<Int8>!) -> ICAError
@discardableResult
func ICDScannerDisconnectUSBDevice(_ locationID: UInt32) -> ICAError
@discardableResult
func ICDScannerDisconnectUSBDeviceWithIORegPath(_ locationID: UInt32, _ ioregPath: UnsafeMutablePointer<Int8>!) -> ICAError
@discardableResult
func ICDScannerConnectFWDevice(_ guid: UInt64) -> ICAError
@discardableResult
func ICDScannerConnectFWDeviceWithIORegPath(_ guid: UInt64, _ ioregPath: UnsafeMutablePointer<Int8>!) -> ICAError
@discardableResult
func ICDScannerDisconnectFWDevice(_ guid: UInt64) -> ICAError
@discardableResult
func ICDScannerDisconnectFWDeviceWithIORegPath(_ guid: UInt64, _ ioregPath: UnsafeMutablePointer<Int8>!) -> ICAError
@discardableResult
func ICDScannerConnectBluetoothDevice(_ params: CFDictionary!) -> ICAError
@discardableResult
func ICDScannerDisconnectBluetoothDevice(_ params: CFDictionary!) -> ICAError
@discardableResult
func ICDScannerConnectTCPIPDevice(_ params: CFDictionary!) -> ICAError
@discardableResult
func ICDScannerDisconnectTCPIPDevice(_ params: CFDictionary!) -> ICAError
