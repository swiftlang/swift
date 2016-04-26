
class DRDeviceRef {
}
@available(OSX 10.2, *)
@discardableResult
func DRDeviceGetTypeID() -> CFTypeID
@available(OSX 10.2, *)
@discardableResult
func DRCopyDeviceArray() -> Unmanaged<CFArray>!
@available(OSX 10.2, *)
@discardableResult
func DRDeviceCopyDeviceForBSDName(_ name: CFString!) -> Unmanaged<DRDeviceRef>!
@available(OSX 10.2, *)
@discardableResult
func DRDeviceCopyDeviceForIORegistryEntryPath(_ path: CFString!) -> Unmanaged<DRDeviceRef>!
@available(OSX 10.2, *)
@discardableResult
func DRDeviceIsValid(_ device: DRDeviceRef!) -> Bool
@available(OSX 10.2, *)
@discardableResult
func DRDeviceOpenTray(_ device: DRDeviceRef!) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func DRDeviceCloseTray(_ device: DRDeviceRef!) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func DRDeviceEjectMedia(_ device: DRDeviceRef!) -> OSStatus
@available(OSX 10.2, *)
func DRDeviceAcquireMediaReservation(_ device: DRDeviceRef!)
@available(OSX 10.2, *)
func DRDeviceReleaseMediaReservation(_ device: DRDeviceRef!)
@available(OSX 10.2, *)
@discardableResult
func DRDeviceAcquireExclusiveAccess(_ device: DRDeviceRef!) -> OSStatus
@available(OSX 10.2, *)
func DRDeviceReleaseExclusiveAccess(_ device: DRDeviceRef!)
@available(OSX 10.2, *)
@discardableResult
func DRDeviceCopyInfo(_ device: DRDeviceRef!) -> Unmanaged<CFDictionary>!
@available(OSX 10.2, *)
@discardableResult
func DRDeviceCopyStatus(_ device: DRDeviceRef!) -> Unmanaged<CFDictionary>!
@available(OSX 10.2, *)
let kDRDeviceAppearedNotification: CFString!
@available(OSX 10.2, *)
let kDRDeviceDisappearedNotification: CFString!
@available(OSX 10.2, *)
let kDRDeviceStatusChangedNotification: CFString!
@available(OSX 10.2, *)
let kDRDeviceSupportLevelKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceIORegistryEntryPathKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceVendorNameKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceProductNameKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceFirmwareRevisionKey: CFString!
@available(OSX 10.2, *)
let kDRDevicePhysicalInterconnectKey: CFString!
@available(OSX 10.2, *)
let kDRDevicePhysicalInterconnectLocationKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceWriteCapabilitiesKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceLoadingMechanismCanEjectKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceLoadingMechanismCanInjectKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceLoadingMechanismCanOpenKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceWriteBufferSizeKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceSupportLevelNone: CFString!
@available(OSX 10.3, *)
let kDRDeviceSupportLevelUnsupported: CFString!
@available(OSX 10.2, *)
let kDRDeviceSupportLevelVendorSupported: CFString!
@available(OSX 10.2, *)
let kDRDeviceSupportLevelAppleSupported: CFString!
@available(OSX 10.2, *)
let kDRDeviceSupportLevelAppleShipping: CFString!
@available(OSX 10.2, *)
let kDRDevicePhysicalInterconnectATAPI: CFString!
@available(OSX 10.3, *)
let kDRDevicePhysicalInterconnectFibreChannel: CFString!
@available(OSX 10.2, *)
let kDRDevicePhysicalInterconnectFireWire: CFString!
@available(OSX 10.2, *)
let kDRDevicePhysicalInterconnectUSB: CFString!
@available(OSX 10.2, *)
let kDRDevicePhysicalInterconnectSCSI: CFString!
@available(OSX 10.2, *)
let kDRDevicePhysicalInterconnectLocationInternal: CFString!
@available(OSX 10.2, *)
let kDRDevicePhysicalInterconnectLocationExternal: CFString!
@available(OSX 10.2, *)
let kDRDevicePhysicalInterconnectLocationUnknown: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanWriteKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanWriteCDKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanWriteCDRKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanWriteCDRWKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanWriteDVDKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanWriteDVDRKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteDVDRDualLayerKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanWriteDVDRWKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteDVDRWDualLayerKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanWriteDVDRAMKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceCanWriteDVDPlusRKey: CFString!
@available(OSX 10.4, *)
let kDRDeviceCanWriteDVDPlusRDoubleLayerKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceCanWriteDVDPlusRWKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteDVDPlusRWDoubleLayerKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteBDKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteBDRKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteBDREKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteHDDVDKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteHDDVDRKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteHDDVDRDualLayerKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteHDDVDRAMKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteHDDVDRWKey: CFString!
@available(OSX 10.5, *)
let kDRDeviceCanWriteHDDVDRWDualLayerKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanWriteCDTextKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceCanWriteIndexPointsKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceCanWriteISRCKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceCanWriteCDTAOKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceCanWriteCDSAOKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceCanWriteCDRawKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceCanWriteDVDDAOKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanTestWriteCDKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanTestWriteDVDKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanUnderrunProtectCDKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCanUnderrunProtectDVDKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceIsBusyKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceIsTrayOpenKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMaximumWriteSpeedKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceCurrentWriteSpeedKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaStateKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaInfoKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceBurnSpeedsKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceTrackRefsKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceTrackInfoKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaStateMediaPresent: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaStateInTransition: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaStateNone: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaBSDNameKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaIsBlankKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaIsAppendableKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceMediaIsOverwritableKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaIsErasableKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaIsReservedKey: CFString!
@available(OSX 10.3, *)
let kDRDeviceMediaBlocksOverwritableKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaBlocksFreeKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaBlocksUsedKey: CFString!
@available(OSX 10.4, *)
let kDRDeviceMediaDoubleLayerL0DataZoneBlocksKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaTrackCountKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaSessionCountKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaClassKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaTypeKey: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaClassCD: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaClassDVD: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaClassBD: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaClassHDDVD: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaClassUnknown: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaTypeCDROM: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaTypeCDR: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaTypeCDRW: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaTypeDVDROM: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaTypeDVDRAM: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaTypeDVDR: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeDVDRDualLayer: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaTypeDVDRW: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeDVDRWDualLayer: CFString!
@available(OSX 10.3, *)
let kDRDeviceMediaTypeDVDPlusR: CFString!
@available(OSX 10.4, *)
let kDRDeviceMediaTypeDVDPlusRDoubleLayer: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaTypeDVDPlusRW: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeDVDPlusRWDoubleLayer: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeBDR: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeBDRE: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeBDROM: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeHDDVDROM: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeHDDVDR: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeHDDVDRDualLayer: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeHDDVDRAM: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeHDDVDRW: CFString!
@available(OSX 10.5, *)
let kDRDeviceMediaTypeHDDVDRWDualLayer: CFString!
@available(OSX 10.2, *)
let kDRDeviceMediaTypeUnknown: CFString!
@available(OSX 10.2, *)
let kDRDeviceBurnSpeedCD1x: Float
@available(OSX 10.2, *)
let kDRDeviceBurnSpeedDVD1x: Float
@available(OSX 10.5, *)
let kDRDeviceBurnSpeedBD1x: Float
@available(OSX 10.5, *)
let kDRDeviceBurnSpeedHDDVD1x: Float
@available(OSX 10.2, *)
let kDRDeviceBurnSpeedMax: Float
@available(OSX 10.5, *)
@discardableResult
func DRDeviceKPSForXFactor(_ deviceOrMediaType: DRType!, _ xfactor: Float) -> Float
@available(OSX 10.5, *)
@discardableResult
func DRDeviceXFactorForKPS(_ deviceOrMediaType: DRType!, _ kps: Float) -> Float
