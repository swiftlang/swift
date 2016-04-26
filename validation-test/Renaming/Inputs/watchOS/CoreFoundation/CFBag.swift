
typealias CFBagRetainCallBack = @convention(c) (CFAllocator!, UnsafePointer<Void>!) -> UnsafePointer<Void>!
typealias CFBagReleaseCallBack = @convention(c) (CFAllocator!, UnsafePointer<Void>!) -> Void
typealias CFBagCopyDescriptionCallBack = @convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!
typealias CFBagEqualCallBack = @convention(c) (UnsafePointer<Void>!, UnsafePointer<Void>!) -> DarwinBoolean
typealias CFBagHashCallBack = @convention(c) (UnsafePointer<Void>!) -> CFHashCode
struct CFBagCallBacks {
  var version: CFIndex
  var retain: CFBagRetainCallBack!
  var release: CFBagReleaseCallBack!
  var copyDescription: CFBagCopyDescriptionCallBack!
  var equal: CFBagEqualCallBack!
  var hash: CFBagHashCallBack!
  init()
  init(version version: CFIndex, retain retain: CFBagRetainCallBack!, release release: CFBagReleaseCallBack!, copyDescription copyDescription: CFBagCopyDescriptionCallBack!, equal equal: CFBagEqualCallBack!, hash hash: CFBagHashCallBack!)
}
let kCFTypeBagCallBacks: CFBagCallBacks
let kCFCopyStringBagCallBacks: CFBagCallBacks
typealias CFBagApplierFunction = @convention(c) (UnsafePointer<Void>!, UnsafeMutablePointer<Void>!) -> Void
class CFBag {
}
class CFMutableBag {
}
@discardableResult
func CFBagGetTypeID() -> CFTypeID
@discardableResult
func CFBagCreate(_ allocator: CFAllocator!, _ values: UnsafeMutablePointer<UnsafePointer<Void>?>!, _ numValues: CFIndex, _ callBacks: UnsafePointer<CFBagCallBacks>!) -> CFBag!
@discardableResult
func CFBagCreateCopy(_ allocator: CFAllocator!, _ theBag: CFBag!) -> CFBag!
@discardableResult
func CFBagCreateMutable(_ allocator: CFAllocator!, _ capacity: CFIndex, _ callBacks: UnsafePointer<CFBagCallBacks>!) -> CFMutableBag!
@discardableResult
func CFBagCreateMutableCopy(_ allocator: CFAllocator!, _ capacity: CFIndex, _ theBag: CFBag!) -> CFMutableBag!
@discardableResult
func CFBagGetCount(_ theBag: CFBag!) -> CFIndex
@discardableResult
func CFBagGetCountOfValue(_ theBag: CFBag!, _ value: UnsafePointer<Void>!) -> CFIndex
@discardableResult
func CFBagContainsValue(_ theBag: CFBag!, _ value: UnsafePointer<Void>!) -> Bool
@discardableResult
func CFBagGetValue(_ theBag: CFBag!, _ value: UnsafePointer<Void>!) -> UnsafePointer<Void>!
@discardableResult
func CFBagGetValueIfPresent(_ theBag: CFBag!, _ candidate: UnsafePointer<Void>!, _ value: UnsafeMutablePointer<UnsafePointer<Void>?>!) -> Bool
func CFBagGetValues(_ theBag: CFBag!, _ values: UnsafeMutablePointer<UnsafePointer<Void>?>!)
func CFBagApplyFunction(_ theBag: CFBag!, _ applier: CFBagApplierFunction!, _ context: UnsafeMutablePointer<Void>!)
func CFBagAddValue(_ theBag: CFMutableBag!, _ value: UnsafePointer<Void>!)
func CFBagReplaceValue(_ theBag: CFMutableBag!, _ value: UnsafePointer<Void>!)
func CFBagSetValue(_ theBag: CFMutableBag!, _ value: UnsafePointer<Void>!)
func CFBagRemoveValue(_ theBag: CFMutableBag!, _ value: UnsafePointer<Void>!)
func CFBagRemoveAllValues(_ theBag: CFMutableBag!)
