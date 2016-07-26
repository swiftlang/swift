
class DRBurnSession {
}
@available(OSX 10.3, *)
@discardableResult
func DRBurnSessionGetTypeID() -> CFTypeID
@available(OSX 10.3, *)
@discardableResult
func DRBurnSessionCreate() -> Unmanaged<DRBurnSession>!
@available(OSX 10.3, *)
func DRBurnSessionSetBurn(_ burnSession: DRBurnSession!, _ burn: DRBurnRef!)
@available(OSX 10.3, *)
@discardableResult
func DRBurnSessionGetBurn(_ burnSession: DRBurnSession!) -> Unmanaged<DRBurnRef>!
var kDRBurnSessionOK: Int { get }
var kDRBurnSessionCancel: Int { get }
var kBurnSessionSetupDialogOptionsCurrentVersion: Int { get }
var kBurnSessionSetupDialogDefaultOptions: UInt32 { get }
var kBurnSessionSetupDialogForceClosedDiscs: UInt32 { get }
var kBurnSessionSetupDialogDontHandleReservations: UInt32 { get }
var kBurnSessionSetupDialogAllowTestBurns: UInt32 { get }
typealias DRBurnSessionSetupDialogOptionFlags = UInt32
struct DRBurnSessionSetupDialogOptions {
  var version: UInt32
  var dialogOptionFlags: DRBurnSessionSetupDialogOptionFlags
  var defaultButtonTitle: Unmanaged<CFString>!
  init()
  init(version version: UInt32, dialogOptionFlags dialogOptionFlags: DRBurnSessionSetupDialogOptionFlags, defaultButtonTitle defaultButtonTitle: Unmanaged<CFString>!)
}
var kDRBurnSessionSetupCallbacksCurrentVersion: Int { get }
typealias DRBurnSessionDeviceCheckProcPtr = @convention(c) (DRBurnSession!, DRDeviceRef!) -> DarwinBoolean
typealias DRBurnSessionMediaCheckProcPtr = @convention(c) (DRBurnSession!, DRDeviceRef!, UnsafeMutablePointer<Unmanaged<CFString>?>!) -> DarwinBoolean
typealias DRBurnSessionDeviceSelectionNotificationProcPtr = @convention(c) (DRBurnSession!, DRDeviceRef!) -> Void
struct DRBurnSessionSetupCallbacks {
  var version: UInt32
  var deviceShouldBeTarget: DRBurnSessionDeviceCheckProcPtr!
  var containsSuitableMedia: DRBurnSessionMediaCheckProcPtr!
  var deviceSelectionChanged: DRBurnSessionDeviceSelectionNotificationProcPtr!
  init()
  init(version version: UInt32, deviceShouldBeTarget deviceShouldBeTarget: DRBurnSessionDeviceCheckProcPtr!, containsSuitableMedia containsSuitableMedia: DRBurnSessionMediaCheckProcPtr!, deviceSelectionChanged deviceSelectionChanged: DRBurnSessionDeviceSelectionNotificationProcPtr!)
}
@available(OSX 10.3, *)
@discardableResult
func DRBurnSessionSetupDialog(_ burnSession: DRBurnSession!, _ options: UnsafeMutablePointer<DRBurnSessionSetupDialogOptions>!, _ setupCallbacks: UnsafeMutablePointer<DRBurnSessionSetupCallbacks>!) -> Int8
var kDRBurnProgressSetupCallbacksCurrentVersion: Int { get }
typealias DRBurnSessionProgressBeginNotificationProcPtr = @convention(c) (DRBurnSession!) -> Void
typealias DRBurnSessionProgressFinishNotificationProcPtr = @convention(c) (DRBurnSession!) -> Void
typealias DRBurnSessionBurnCompleteProcPtr = @convention(c) (DRBurnSession!, DRBurnRef!) -> DarwinBoolean
struct DRBurnSessionProgressCallbacks {
  var version: UInt32
  var progressWillBegin: DRBurnSessionProgressBeginNotificationProcPtr!
  var progressDidFinish: DRBurnSessionProgressFinishNotificationProcPtr!
  var burnDidFinish: DRBurnSessionBurnCompleteProcPtr!
  init()
  init(version version: UInt32, progressWillBegin progressWillBegin: DRBurnSessionProgressBeginNotificationProcPtr!, progressDidFinish progressDidFinish: DRBurnSessionProgressFinishNotificationProcPtr!, burnDidFinish burnDidFinish: DRBurnSessionBurnCompleteProcPtr!)
}
var kBurnSessionProgressDialogOptionsCurrentVersion: Int { get }
var kBurnSessionProgressDialogDefaultOptions: Int { get }
var kBurnSessionProgressDialogDisplayVerboseProgress: Int { get }
typealias DRBurnSessionProgressDialogOptionFlags = UInt32
struct DRBurnSessionProgressDialogOptions {
  var version: UInt32
  var dialogOptionFlags: DRBurnSessionProgressDialogOptionFlags
  var description: Unmanaged<CFString>!
  init()
  init(version version: UInt32, dialogOptionFlags dialogOptionFlags: DRBurnSessionProgressDialogOptionFlags, description description: Unmanaged<CFString>!)
}
@available(OSX 10.3, *)
func DRBurnSessionBeginProgressDialog(_ burnSession: DRBurnSession!, _ layout: CFTypeRef!, _ options: UnsafeMutablePointer<DRBurnSessionProgressDialogOptions>!, _ progressCallbacks: UnsafeMutablePointer<DRBurnSessionProgressCallbacks>!)
