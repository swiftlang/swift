
class DRBurnRef {
}
@available(OSX 10.2, *)
@discardableResult
func DRBurnGetTypeID() -> CFTypeID
@available(OSX 10.2, *)
@discardableResult
func DRBurnCreate(_ device: DRDeviceRef!) -> Unmanaged<DRBurnRef>!
@available(OSX 10.2, *)
@discardableResult
func DRBurnWriteLayout(_ burn: DRBurnRef!, _ layout: CFTypeRef!) -> OSStatus
@available(OSX 10.2, *)
func DRBurnAbort(_ burn: DRBurnRef!)
@available(OSX 10.2, *)
@discardableResult
func DRBurnCopyStatus(_ burn: DRBurnRef!) -> Unmanaged<CFDictionary>!
@available(OSX 10.2, *)
@discardableResult
func DRBurnGetDevice(_ burn: DRBurnRef!) -> Unmanaged<DRDeviceRef>!
@available(OSX 10.2, *)
func DRBurnSetProperties(_ burn: DRBurnRef!, _ properties: CFDictionary!)
@available(OSX 10.2, *)
@discardableResult
func DRBurnGetProperties(_ burn: DRBurnRef!) -> Unmanaged<CFDictionary>!
@available(OSX 10.2, *)
let kDRBurnStatusChangedNotification: CFString!
@available(OSX 10.2, *)
let kDRBurnRequestedSpeedKey: CFString!
@available(OSX 10.2, *)
let kDRBurnAppendableKey: CFString!
@available(OSX 10.3, *)
let kDRBurnOverwriteDiscKey: CFString!
@available(OSX 10.2, *)
let kDRBurnVerifyDiscKey: CFString!
@available(OSX 10.2, *)
let kDRBurnCompletionActionKey: CFString!
@available(OSX 10.2, *)
let kDRBurnUnderrunProtectionKey: CFString!
@available(OSX 10.2, *)
let kDRBurnTestingKey: CFString!
@available(OSX 10.2, *)
let kDRSynchronousBehaviorKey: CFString!
@available(OSX 10.3, *)
let kDRBurnFailureActionKey: CFString!
@available(OSX 10.3, *)
let kDRMediaCatalogNumberKey: CFString!
@available(OSX 10.4, *)
let kDRBurnDoubleLayerL0DataZoneBlocksKey: CFString!
@available(OSX 10.3, *)
let kDRBurnStrategyKey: CFString!
@available(OSX 10.3, *)
let kDRBurnStrategyIsRequiredKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextKey: CFString!
@available(OSX 10.2, *)
let kDRBurnCompletionActionEject: CFString!
@available(OSX 10.2, *)
let kDRBurnCompletionActionMount: CFString!
@available(OSX 10.3, *)
let kDRBurnFailureActionEject: CFString!
@available(OSX 10.3, *)
let kDRBurnFailureActionNone: CFString!
@available(OSX 10.3, *)
let kDRBurnStrategyCDTAO: CFString!
@available(OSX 10.3, *)
let kDRBurnStrategyCDSAO: CFString!
@available(OSX 10.3, *)
let kDRBurnStrategyDVDDAO: CFString!
@available(OSX 10.5, *)
let kDRBurnStrategyBDDAO: CFString!
