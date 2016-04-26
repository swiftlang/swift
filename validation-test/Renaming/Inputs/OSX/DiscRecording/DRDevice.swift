
class DRDevice : NSObject {
  @discardableResult
  class func devices() -> [AnyObject]!
  /*not inherited*/ init!(forBSDName bsdName: String!)
  /*not inherited*/ init!(forIORegistryEntryPath path: String!)
  @discardableResult
  func isValid() -> Bool
  @discardableResult
  func info() -> [NSObject : AnyObject]!
  @discardableResult
  func status() -> [NSObject : AnyObject]!
  @discardableResult
  func openTray() -> Bool
  @discardableResult
  func closeTray() -> Bool
  @discardableResult
  func ejectMedia() -> Bool
  @discardableResult
  func acquireExclusiveAccess() -> Bool
  func releaseExclusiveAccess()
  func acquireMediaReservation()
  func releaseMediaReservation()
  @discardableResult
  func isEqual(to otherDevice: DRDevice!) -> Bool
}
extension DRDevice {
  @discardableResult
  func writesCD() -> Bool
  @discardableResult
  func writesDVD() -> Bool
  @discardableResult
  func displayName() -> String!
  @discardableResult
  func ioRegistryEntryPath() -> String!
}
extension DRDevice {
  @discardableResult
  func mediaIsPresent() -> Bool
  @discardableResult
  func mediaIsTransitioning() -> Bool
  @discardableResult
  func mediaIsBusy() -> Bool
  @discardableResult
  func mediaType() -> String!
  @discardableResult
  func mediaIsBlank() -> Bool
  @discardableResult
  func mediaIsAppendable() -> Bool
  @discardableResult
  func mediaIsOverwritable() -> Bool
  @discardableResult
  func mediaIsErasable() -> Bool
  @discardableResult
  func mediaIsReserved() -> Bool
  @discardableResult
  func mediaSpaceOverwritable() -> DRMSF!
  @discardableResult
  func mediaSpaceUsed() -> DRMSF!
  @discardableResult
  func mediaSpaceFree() -> DRMSF!
  @discardableResult
  func trayIsOpen() -> Bool
  @discardableResult
  func bsdName() -> String!
}
@available(OSX 10.2, *)
let DRDeviceBurnSpeedCD1x: Float
@available(OSX 10.2, *)
let DRDeviceBurnSpeedDVD1x: Float
@available(OSX 10.5, *)
let DRDeviceBurnSpeedBD1x: Float
@available(OSX 10.5, *)
let DRDeviceBurnSpeedHDDVD1x: Float
@available(OSX 10.2, *)
let DRDeviceBurnSpeedMax: Float
@available(OSX 10.2, *)
let DRDeviceAppearedNotification: String
@available(OSX 10.2, *)
let DRDeviceDisappearedNotification: String
@available(OSX 10.2, *)
let DRDeviceStatusChangedNotification: String
@available(OSX 10.2, *)
let DRDeviceSupportLevelKey: String
@available(OSX 10.2, *)
let DRDeviceIORegistryEntryPathKey: String
@available(OSX 10.2, *)
let DRDeviceWriteCapabilitiesKey: String
@available(OSX 10.2, *)
let DRDeviceVendorNameKey: String
@available(OSX 10.2, *)
let DRDeviceProductNameKey: String
@available(OSX 10.2, *)
let DRDeviceFirmwareRevisionKey: String
@available(OSX 10.2, *)
let DRDevicePhysicalInterconnectKey: String
@available(OSX 10.2, *)
let DRDevicePhysicalInterconnectLocationKey: String
@available(OSX 10.3, *)
let DRDeviceLoadingMechanismCanEjectKey: String
@available(OSX 10.3, *)
let DRDeviceLoadingMechanismCanInjectKey: String
@available(OSX 10.3, *)
let DRDeviceLoadingMechanismCanOpenKey: String
@available(OSX 10.3, *)
let DRDeviceWriteBufferSizeKey: String
@available(OSX 10.2, *)
let DRDeviceSupportLevelNone: String
@available(OSX 10.3, *)
let DRDeviceSupportLevelUnsupported: String
@available(OSX 10.2, *)
let DRDeviceSupportLevelVendorSupported: String
@available(OSX 10.2, *)
let DRDeviceSupportLevelAppleSupported: String
@available(OSX 10.2, *)
let DRDeviceSupportLevelAppleShipping: String
@available(OSX 10.2, *)
let DRDevicePhysicalInterconnectATAPI: String
@available(OSX 10.3, *)
let DRDevicePhysicalInterconnectFibreChannel: String
@available(OSX 10.2, *)
let DRDevicePhysicalInterconnectFireWire: String
@available(OSX 10.2, *)
let DRDevicePhysicalInterconnectSCSI: String
@available(OSX 10.2, *)
let DRDevicePhysicalInterconnectUSB: String
@available(OSX 10.2, *)
let DRDevicePhysicalInterconnectLocationInternal: String
@available(OSX 10.2, *)
let DRDevicePhysicalInterconnectLocationExternal: String
@available(OSX 10.2, *)
let DRDevicePhysicalInterconnectLocationUnknown: String
@available(OSX 10.2, *)
let DRDeviceCanWriteKey: String
@available(OSX 10.2, *)
let DRDeviceCanWriteCDKey: String
@available(OSX 10.2, *)
let DRDeviceCanWriteCDRKey: String
@available(OSX 10.2, *)
let DRDeviceCanWriteCDRWKey: String
@available(OSX 10.2, *)
let DRDeviceCanWriteDVDKey: String
@available(OSX 10.2, *)
let DRDeviceCanWriteDVDRKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteDVDRDualLayerKey: String
@available(OSX 10.2, *)
let DRDeviceCanWriteDVDRWKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteDVDRWDualLayerKey: String
@available(OSX 10.2, *)
let DRDeviceCanWriteDVDRAMKey: String
@available(OSX 10.3, *)
let DRDeviceCanWriteDVDPlusRKey: String
@available(OSX 10.4, *)
let DRDeviceCanWriteDVDPlusRDoubleLayerKey: String
@available(OSX 10.3, *)
let DRDeviceCanWriteDVDPlusRWKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteDVDPlusRWDoubleLayerKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteBDKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteBDRKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteBDREKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteHDDVDKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteHDDVDRKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteHDDVDRDualLayerKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteHDDVDRAMKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteHDDVDRWKey: String
@available(OSX 10.5, *)
let DRDeviceCanWriteHDDVDRWDualLayerKey: String
@available(OSX 10.2, *)
let DRDeviceCanWriteCDTextKey: String
@available(OSX 10.3, *)
let DRDeviceCanWriteIndexPointsKey: String
@available(OSX 10.3, *)
let DRDeviceCanWriteISRCKey: String
@available(OSX 10.3, *)
let DRDeviceCanWriteCDTAOKey: String
@available(OSX 10.3, *)
let DRDeviceCanWriteCDSAOKey: String
@available(OSX 10.3, *)
let DRDeviceCanWriteCDRawKey: String
@available(OSX 10.3, *)
let DRDeviceCanWriteDVDDAOKey: String
@available(OSX 10.2, *)
let DRDeviceCanTestWriteCDKey: String
@available(OSX 10.2, *)
let DRDeviceCanTestWriteDVDKey: String
@available(OSX 10.2, *)
let DRDeviceCanUnderrunProtectCDKey: String
@available(OSX 10.2, *)
let DRDeviceCanUnderrunProtectDVDKey: String
@available(OSX 10.2, *)
let DRDeviceIsBusyKey: String
@available(OSX 10.2, *)
let DRDeviceIsTrayOpenKey: String
@available(OSX 10.2, *)
let DRDeviceMaximumWriteSpeedKey: String
@available(OSX 10.2, *)
let DRDeviceCurrentWriteSpeedKey: String
@available(OSX 10.2, *)
let DRDeviceMediaStateKey: String
@available(OSX 10.2, *)
let DRDeviceMediaInfoKey: String
@available(OSX 10.2, *)
let DRDeviceBurnSpeedsKey: String
@available(OSX 10.3, *)
let DRDeviceTrackRefsKey: String
@available(OSX 10.3, *)
let DRDeviceTrackInfoKey: String
@available(OSX 10.2, *)
let DRDeviceMediaStateMediaPresent: String
@available(OSX 10.2, *)
let DRDeviceMediaStateInTransition: String
@available(OSX 10.2, *)
let DRDeviceMediaStateNone: String
@available(OSX 10.2, *)
let DRDeviceMediaBSDNameKey: String
@available(OSX 10.2, *)
let DRDeviceMediaIsBlankKey: String
@available(OSX 10.2, *)
let DRDeviceMediaIsAppendableKey: String
@available(OSX 10.3, *)
let DRDeviceMediaIsOverwritableKey: String
@available(OSX 10.2, *)
let DRDeviceMediaIsErasableKey: String
@available(OSX 10.2, *)
let DRDeviceMediaIsReservedKey: String
@available(OSX 10.3, *)
let DRDeviceMediaOverwritableSpaceKey: String
@available(OSX 10.2, *)
let DRDeviceMediaFreeSpaceKey: String
@available(OSX 10.2, *)
let DRDeviceMediaUsedSpaceKey: String
@available(OSX 10.3, *)
let DRDeviceMediaBlocksOverwritableKey: String
@available(OSX 10.2, *)
let DRDeviceMediaBlocksFreeKey: String
@available(OSX 10.2, *)
let DRDeviceMediaBlocksUsedKey: String
@available(OSX 10.4, *)
let DRDeviceMediaDoubleLayerL0DataZoneBlocksKey: String
@available(OSX 10.2, *)
let DRDeviceMediaTrackCountKey: String
@available(OSX 10.2, *)
let DRDeviceMediaSessionCountKey: String
@available(OSX 10.2, *)
let DRDeviceMediaClassKey: String
@available(OSX 10.2, *)
let DRDeviceMediaTypeKey: String
@available(OSX 10.2, *)
let DRDeviceMediaClassCD: String
@available(OSX 10.2, *)
let DRDeviceMediaClassDVD: String
@available(OSX 10.5, *)
let DRDeviceMediaClassBD: String
@available(OSX 10.5, *)
let DRDeviceMediaClassHDDVD: String
@available(OSX 10.2, *)
let DRDeviceMediaClassUnknown: String
@available(OSX 10.2, *)
let DRDeviceMediaTypeCDROM: String
@available(OSX 10.2, *)
let DRDeviceMediaTypeCDR: String
@available(OSX 10.2, *)
let DRDeviceMediaTypeCDRW: String
@available(OSX 10.2, *)
let DRDeviceMediaTypeDVDROM: String
@available(OSX 10.2, *)
let DRDeviceMediaTypeDVDRAM: String
@available(OSX 10.2, *)
let DRDeviceMediaTypeDVDR: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeDVDRDualLayer: String
@available(OSX 10.2, *)
let DRDeviceMediaTypeDVDRW: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeDVDRWDualLayer: String
@available(OSX 10.3, *)
let DRDeviceMediaTypeDVDPlusR: String
@available(OSX 10.4, *)
let DRDeviceMediaTypeDVDPlusRDoubleLayer: String
@available(OSX 10.3, *)
let DRDeviceMediaTypeDVDPlusRW: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeDVDPlusRWDoubleLayer: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeBDR: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeBDRE: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeBDROM: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeHDDVDROM: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeHDDVDR: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeHDDVDRDualLayer: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeHDDVDRAM: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeHDDVDRW: String
@available(OSX 10.5, *)
let DRDeviceMediaTypeHDDVDRWDualLayer: String
@available(OSX 10.2, *)
let DRDeviceMediaTypeUnknown: String
