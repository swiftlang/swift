
class DREraseSession {
}
@available(OSX 10.3, *)
@discardableResult
func DREraseSessionGetTypeID() -> CFTypeID
@available(OSX 10.3, *)
@discardableResult
func DREraseSessionCreate() -> Unmanaged<DREraseSession>!
@available(OSX 10.3, *)
func DREraseSessionSetErase(_ eraseSession: DREraseSession!, _ erase: DREraseRef!)
@available(OSX 10.3, *)
@discardableResult
func DREraseSessionGetErase(_ eraseSession: DREraseSession!) -> Unmanaged<DREraseRef>!
var kDREraseSessionOK: Int { get }
var kDREraseSessionCancel: Int { get }
var kEraseSessionSetupDialogOptionsCurrentVersion: Int { get }
var kEraseSessionSetupDialogDefaultOptions: Int { get }
var kEraseSessionSetupDialogDontHandleReservations: Int { get }
typealias DREraseSessionSetupDialogOptionFlags = UInt32
struct DREraseSessionSetupDialogOptions {
  var version: UInt32
  var dialogOptionFlags: DREraseSessionSetupDialogOptionFlags
  init()
  init(version version: UInt32, dialogOptionFlags dialogOptionFlags: DREraseSessionSetupDialogOptionFlags)
}
var kDREraseSessionSetupCallbacksCurrentVersion: Int { get }
typealias DREraseSessionDeviceCheckProcPtr = @convention(c) (DREraseSession!, DRDeviceRef!) -> DarwinBoolean
typealias DREraseSessionMediaCheckProcPtr = @convention(c) (DREraseSession!, DRDeviceRef!, UnsafeMutablePointer<Unmanaged<CFString>?>!) -> DarwinBoolean
typealias DREraseSessionDeviceSelectionNotificationProcPtr = @convention(c) (DREraseSession!, DRDeviceRef!) -> Void
struct DREraseSessionSetupCallbacks {
  var version: UInt32
  var deviceShouldBeTarget: DREraseSessionDeviceCheckProcPtr!
  var containsSuitableMedia: DREraseSessionMediaCheckProcPtr!
  var deviceSelectionChanged: DREraseSessionDeviceSelectionNotificationProcPtr!
  init()
  init(version version: UInt32, deviceShouldBeTarget deviceShouldBeTarget: DREraseSessionDeviceCheckProcPtr!, containsSuitableMedia containsSuitableMedia: DREraseSessionMediaCheckProcPtr!, deviceSelectionChanged deviceSelectionChanged: DREraseSessionDeviceSelectionNotificationProcPtr!)
}
@available(OSX 10.3, *)
@discardableResult
func DREraseSessionSetupDialog(_ eraseSession: DREraseSession!, _ options: UnsafeMutablePointer<DREraseSessionSetupDialogOptions>!, _ setupCallbacks: UnsafeMutablePointer<DREraseSessionSetupCallbacks>!) -> Int8
var kDREraseProgressSetupCallbacksCurrentVersion: Int { get }
typealias DREraseSessionProgressBeginNotificationProcPtr = @convention(c) (DREraseSession!) -> Void
typealias DREraseSessionProgressFinishNotificationProcPtr = @convention(c) (DREraseSession!) -> Void
typealias DREraseSessionEraseCompleteProcPtr = @convention(c) (DREraseSession!, DREraseRef!) -> DarwinBoolean
struct DREraseSessionProgressCallbacks {
  var version: UInt32
  var progressWillBegin: DREraseSessionProgressBeginNotificationProcPtr!
  var progressDidFinish: DREraseSessionProgressFinishNotificationProcPtr!
  var eraseDidFinish: DREraseSessionEraseCompleteProcPtr!
  init()
  init(version version: UInt32, progressWillBegin progressWillBegin: DREraseSessionProgressBeginNotificationProcPtr!, progressDidFinish progressDidFinish: DREraseSessionProgressFinishNotificationProcPtr!, eraseDidFinish eraseDidFinish: DREraseSessionEraseCompleteProcPtr!)
}
var kEraseSessionProgressDialogOptionsCurrentVersion: Int { get }
var kEraseSessionProgressDialogDefaultOptions: Int { get }
typealias DREraseSessionProgressDialogOptionFlags = UInt32
struct DREraseSessionProgressDialogOptions {
  var version: UInt32
  var dialogOptionFlags: DREraseSessionProgressDialogOptionFlags
  var description: Unmanaged<CFString>!
  init()
  init(version version: UInt32, dialogOptionFlags dialogOptionFlags: DREraseSessionProgressDialogOptionFlags, description description: Unmanaged<CFString>!)
}
@available(OSX 10.3, *)
func DREraseSessionBeginProgressDialog(_ eraseSession: DREraseSession!, _ options: UnsafeMutablePointer<DREraseSessionProgressDialogOptions>!, _ progressCallbacks: UnsafeMutablePointer<DREraseSessionProgressCallbacks>!)
