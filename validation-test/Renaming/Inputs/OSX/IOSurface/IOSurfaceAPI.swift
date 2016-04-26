
class IOSurface {
}
typealias IOSurfaceID = UInt32
@available(OSX 10.6, *)
let kIOSurfaceAllocSize: CFString
@available(OSX 10.6, *)
let kIOSurfaceWidth: CFString
@available(OSX 10.6, *)
let kIOSurfaceHeight: CFString
@available(OSX 10.6, *)
let kIOSurfaceBytesPerRow: CFString
@available(OSX 10.6, *)
let kIOSurfaceBytesPerElement: CFString
@available(OSX 10.6, *)
let kIOSurfaceElementWidth: CFString
@available(OSX 10.6, *)
let kIOSurfaceElementHeight: CFString
@available(OSX 10.6, *)
let kIOSurfaceOffset: CFString
@available(OSX 10.6, *)
let kIOSurfacePlaneInfo: CFString
@available(OSX 10.6, *)
let kIOSurfacePlaneWidth: CFString
@available(OSX 10.6, *)
let kIOSurfacePlaneHeight: CFString
@available(OSX 10.6, *)
let kIOSurfacePlaneBytesPerRow: CFString
@available(OSX 10.6, *)
let kIOSurfacePlaneOffset: CFString
@available(OSX 10.6, *)
let kIOSurfacePlaneSize: CFString
@available(OSX 10.6, *)
let kIOSurfacePlaneBase: CFString
@available(OSX 10.6, *)
let kIOSurfacePlaneBytesPerElement: CFString
@available(OSX 10.6, *)
let kIOSurfacePlaneElementWidth: CFString
@available(OSX 10.6, *)
let kIOSurfacePlaneElementHeight: CFString
@available(OSX 10.6, *)
let kIOSurfaceCacheMode: CFString
@available(OSX, introduced: 10.6, deprecated: 10.11)
let kIOSurfaceIsGlobal: CFString
@available(OSX 10.6, *)
let kIOSurfacePixelFormat: CFString
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetTypeID() -> CFTypeID
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceCreate(_ properties: CFDictionary) -> IOSurface?
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceLookup(_ csid: IOSurfaceID) -> IOSurface?
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetID(_ buffer: IOSurface) -> IOSurfaceID
struct IOSurfaceLockOptions : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var readOnly: IOSurfaceLockOptions { get }
  static var avoidSync: IOSurfaceLockOptions { get }
}
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceLock(_ buffer: IOSurface, _ options: IOSurfaceLockOptions, _ seed: UnsafeMutablePointer<UInt32>?) -> IOReturn
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceUnlock(_ buffer: IOSurface, _ options: IOSurfaceLockOptions, _ seed: UnsafeMutablePointer<UInt32>?) -> IOReturn
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetAllocSize(_ buffer: IOSurface) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetWidth(_ buffer: IOSurface) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetHeight(_ buffer: IOSurface) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetBytesPerElement(_ buffer: IOSurface) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetBytesPerRow(_ buffer: IOSurface) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetBaseAddress(_ buffer: IOSurface) -> UnsafeMutablePointer<Void>
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetElementWidth(_ buffer: IOSurface) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetElementHeight(_ buffer: IOSurface) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetPixelFormat(_ buffer: IOSurface) -> OSType
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetSeed(_ buffer: IOSurface) -> UInt32
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetPlaneCount(_ buffer: IOSurface) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetWidthOfPlane(_ buffer: IOSurface, _ planeIndex: Int) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetHeightOfPlane(_ buffer: IOSurface, _ planeIndex: Int) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetBytesPerElementOfPlane(_ buffer: IOSurface, _ planeIndex: Int) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetBytesPerRowOfPlane(_ buffer: IOSurface, _ planeIndex: Int) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetBaseAddressOfPlane(_ buffer: IOSurface, _ planeIndex: Int) -> UnsafeMutablePointer<Void>
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetElementWidthOfPlane(_ buffer: IOSurface, _ planeIndex: Int) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetElementHeightOfPlane(_ buffer: IOSurface, _ planeIndex: Int) -> Int
@available(OSX 10.6, *)
func IOSurfaceSetValue(_ buffer: IOSurface, _ key: CFString, _ value: CFTypeRef)
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceCopyValue(_ buffer: IOSurface, _ key: CFString) -> CFTypeRef?
@available(OSX 10.6, *)
func IOSurfaceRemoveValue(_ buffer: IOSurface, _ key: CFString)
@available(OSX 10.6, *)
func IOSurfaceSetValues(_ buffer: IOSurface, _ keysAndValues: CFDictionary)
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceCopyAllValues(_ buffer: IOSurface) -> CFDictionary?
@available(OSX 10.6, *)
func IOSurfaceRemoveAllValues(_ buffer: IOSurface)
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceCreateMachPort(_ buffer: IOSurface) -> mach_port_t
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceLookupFromMachPort(_ port: mach_port_t) -> IOSurface?
@available(OSX 10.7, *)
@discardableResult
func IOSurfaceCreateXPCObject(_ aSurface: IOSurface) -> xpc_object_t
@available(OSX 10.7, *)
@discardableResult
func IOSurfaceLookupFromXPCObject(_ xobj: xpc_object_t) -> IOSurface?
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetPropertyMaximum(_ property: CFString) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetPropertyAlignment(_ property: CFString) -> Int
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceAlignProperty(_ property: CFString, _ value: Int) -> Int
@available(OSX 10.6, *)
func IOSurfaceIncrementUseCount(_ buffer: IOSurface)
@available(OSX 10.6, *)
func IOSurfaceDecrementUseCount(_ buffer: IOSurface)
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceGetUseCount(_ buffer: IOSurface) -> Int32
@available(OSX 10.6, *)
@discardableResult
func IOSurfaceIsInUse(_ buffer: IOSurface) -> Bool
