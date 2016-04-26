
var kDRFirstErr: UInt32 { get }
var kDRInternalErr: UInt32 { get }
var kDRDeviceAccessErr: UInt32 { get }
var kDRDeviceBusyErr: UInt32 { get }
var kDRDeviceCommunicationErr: UInt32 { get }
var kDRDeviceInvalidErr: UInt32 { get }
var kDRDeviceNotReadyErr: UInt32 { get }
var kDRDeviceNotSupportedErr: UInt32 { get }
var kDRMediaBusyErr: UInt32 { get }
var kDRMediaNotPresentErr: UInt32 { get }
var kDRMediaNotWritableErr: UInt32 { get }
var kDRMediaNotSupportedErr: UInt32 { get }
var kDRMediaNotBlankErr: UInt32 { get }
var kDRMediaNotErasableErr: UInt32 { get }
var kDRMediaInvalidErr: UInt32 { get }
var kDRBurnUnderrunErr: UInt32 { get }
var kDRBurnNotAllowedErr: UInt32 { get }
var kDRDataProductionErr: UInt32 { get }
var kDRVerificationFailedErr: UInt32 { get }
var kDRTooManyTracksForDVDErr: UInt32 { get }
var kDRBadLayoutErr: UInt32 { get }
var kDRUserCanceledErr: UInt32 { get }
var kDRFunctionNotSupportedErr: UInt32 { get }
var kDRSpeedTestAlreadyRunningErr: UInt32 { get }
var kDRInvalidIndexPointsErr: UInt32 { get }
var kDRDoubleLayerL0DataZoneBlocksParamErr: UInt32 { get }
var kDRDoubleLayerL0AlreadySpecifiedErr: UInt32 { get }
var kDRAudioFileNotSupportedErr: UInt32 { get }
var kDRBurnPowerCalibrationErr: UInt32 { get }
var kDRBurnMediaWriteFailureErr: UInt32 { get }
var kDRTrackReusedErr: UInt32 { get }
var kDRFileModifiedDuringBurnErr: UInt32 { get }
var kDRFileLocationConflictErr: UInt32 { get }
var kDRTooManyNameConflictsErr: UInt32 { get }
var kDRFilesystemsNotSupportedErr: UInt32 { get }
var kDRDeviceBurnStrategyNotAvailableErr: UInt32 { get }
var kDRDeviceCantWriteCDTextErr: UInt32 { get }
var kDRDeviceCantWriteIndexPointsErr: UInt32 { get }
var kDRDeviceCantWriteISRCErr: UInt32 { get }
var kDRDeviceCantWriteSCMSErr: UInt32 { get }
var kDRDevicePreGapLengthNotValidErr: UInt32 { get }
@discardableResult
func DRCopyLocalizedStringForDiscRecordingError(_ osError: OSStatus) -> Unmanaged<CFString>!
@discardableResult
func DRCopyLocalizedStringForSenseCode(_ senseCode: UInt8) -> Unmanaged<CFString>!
@discardableResult
func DRCopyLocalizedStringForAdditionalSense(_ ASC: UInt8, _ ASCQ: UInt8) -> Unmanaged<CFString>!
@available(OSX 10.2, *)
let kDRErrorStatusKey: CFString!
@available(OSX 10.2, *)
let kDRErrorStatusErrorKey: CFString!
@available(OSX 10.2, *)
let kDRErrorStatusErrorStringKey: CFString!
@available(OSX 10.4, *)
let kDRErrorStatusErrorInfoStringKey: CFString!
@available(OSX 10.2, *)
let kDRErrorStatusSenseKey: CFString!
@available(OSX 10.2, *)
let kDRErrorStatusSenseCodeStringKey: CFString!
@available(OSX 10.2, *)
let kDRErrorStatusAdditionalSenseStringKey: CFString!
