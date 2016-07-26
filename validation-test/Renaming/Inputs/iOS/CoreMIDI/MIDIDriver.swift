
typealias MIDIDriverRef = UnsafeMutablePointer<UnsafeMutablePointer<MIDIDriverInterface>?>
typealias MIDIDeviceListRef = MIDIObjectRef
struct MIDIDriverInterface {
  var _reserved: UnsafeMutablePointer<Void>!
  var QueryInterface: (@convention(c) (UnsafeMutablePointer<Void>!, REFIID, UnsafeMutablePointer<LPVOID?>!) -> HRESULT)!
  var AddRef: (@convention(c) (UnsafeMutablePointer<Void>!) -> ULONG)!
  var Release: (@convention(c) (UnsafeMutablePointer<Void>!) -> ULONG)!
  var FindDevices: (@convention(c) (MIDIDriverRef!, MIDIDeviceListRef) -> OSStatus)!
  var Start: (@convention(c) (MIDIDriverRef!, MIDIDeviceListRef) -> OSStatus)!
  var Stop: (@convention(c) (MIDIDriverRef!) -> OSStatus)!
  var Configure: (@convention(c) (MIDIDriverRef!, MIDIDeviceRef) -> OSStatus)!
  var Send: (@convention(c) (MIDIDriverRef!, UnsafePointer<MIDIPacketList>!, UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Void>!) -> OSStatus)!
  var EnableSource: (@convention(c) (MIDIDriverRef!, MIDIEndpointRef, DarwinBoolean) -> OSStatus)!
  var Flush: (@convention(c) (MIDIDriverRef!, MIDIEndpointRef, UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Void>!) -> OSStatus)!
  var Monitor: (@convention(c) (MIDIDriverRef!, MIDIEndpointRef, UnsafePointer<MIDIPacketList>!) -> OSStatus)!
  init()
  init(_reserved _reserved: UnsafeMutablePointer<Void>!, QueryInterface QueryInterface: (@convention(c) (UnsafeMutablePointer<Void>!, REFIID, UnsafeMutablePointer<LPVOID?>!) -> HRESULT)!, AddRef AddRef: (@convention(c) (UnsafeMutablePointer<Void>!) -> ULONG)!, Release Release: (@convention(c) (UnsafeMutablePointer<Void>!) -> ULONG)!, FindDevices FindDevices: (@convention(c) (MIDIDriverRef!, MIDIDeviceListRef) -> OSStatus)!, Start Start: (@convention(c) (MIDIDriverRef!, MIDIDeviceListRef) -> OSStatus)!, Stop Stop: (@convention(c) (MIDIDriverRef!) -> OSStatus)!, Configure Configure: (@convention(c) (MIDIDriverRef!, MIDIDeviceRef) -> OSStatus)!, Send Send: (@convention(c) (MIDIDriverRef!, UnsafePointer<MIDIPacketList>!, UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Void>!) -> OSStatus)!, EnableSource EnableSource: (@convention(c) (MIDIDriverRef!, MIDIEndpointRef, DarwinBoolean) -> OSStatus)!, Flush Flush: (@convention(c) (MIDIDriverRef!, MIDIEndpointRef, UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Void>!) -> OSStatus)!, Monitor Monitor: (@convention(c) (MIDIDriverRef!, MIDIEndpointRef, UnsafePointer<MIDIPacketList>!) -> OSStatus)!)
}
@available(iOS 4.2, *)
@discardableResult
func MIDIDeviceCreate(_ owner: MIDIDriverRef!, _ name: CFString!, _ manufacturer: CFString!, _ model: CFString!, _ outDevice: UnsafeMutablePointer<MIDIDeviceRef>!) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIDeviceDispose(_ device: MIDIDeviceRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIDeviceListGetNumberOfDevices(_ devList: MIDIDeviceListRef) -> Int
@available(iOS 4.2, *)
@discardableResult
func MIDIDeviceListGetDevice(_ devList: MIDIDeviceListRef, _ index0: Int) -> MIDIDeviceRef
@available(iOS 4.2, *)
@discardableResult
func MIDIDeviceListAddDevice(_ devList: MIDIDeviceListRef, _ dev: MIDIDeviceRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIDeviceListDispose(_ devList: MIDIDeviceListRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIEndpointSetRefCons(_ endpt: MIDIEndpointRef, _ ref1: UnsafeMutablePointer<Void>!, _ ref2: UnsafeMutablePointer<Void>!) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIEndpointGetRefCons(_ endpt: MIDIEndpointRef, _ ref1: UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!, _ ref2: UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIGetDriverIORunLoop() -> Unmanaged<CFRunLoop>!
@available(iOS 4.2, *)
@discardableResult
func MIDIGetDriverDeviceList(_ driver: MIDIDriverRef!) -> MIDIDeviceListRef
