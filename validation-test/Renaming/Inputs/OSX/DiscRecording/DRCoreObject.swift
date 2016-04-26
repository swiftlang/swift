
typealias DRType = CFTypeRef
typealias DRRefConRetainCallback = @convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!
typealias DRRefConReleaseCallback = @convention(c) (UnsafePointer<Void>!) -> Void
struct DRRefConCallbacks {
  var version: UInt
  var retain: DRRefConRetainCallback!
  var release: DRRefConReleaseCallback!
  init()
  init(version version: UInt, retain retain: DRRefConRetainCallback!, release release: DRRefConReleaseCallback!)
}
let kDRRefConCFTypeCallbacks: DRRefConCallbacks
@available(OSX 10.2, *)
func DRSetRefCon(_ ref: DRType!, _ refCon: UnsafeMutablePointer<Void>!, _ callbacks: UnsafePointer<DRRefConCallbacks>!)
@available(OSX 10.2, *)
@discardableResult
func DRGetRefCon(_ ref: DRType!) -> UnsafeMutablePointer<Void>!
@available(OSX 10.5, *)
@discardableResult
func DRCopyLocalizedStringForValue(_ value: CFString!) -> Unmanaged<CFString>!
