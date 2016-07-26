
struct CFBinaryHeapCompareContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>!
  var retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!
  var release: (@convention(c) (UnsafePointer<Void>!) -> Void)!
  var copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>!, retain retain: (@convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!)!, release release: (@convention(c) (UnsafePointer<Void>!) -> Void)!, copyDescription copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!)
}
struct CFBinaryHeapCallBacks {
  var version: CFIndex
  var retain: (@convention(c) (CFAllocator!, UnsafePointer<Void>!) -> UnsafePointer<Void>!)!
  var release: (@convention(c) (CFAllocator!, UnsafePointer<Void>!) -> Void)!
  var copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!
  var compare: (@convention(c) (UnsafePointer<Void>!, UnsafePointer<Void>!, UnsafeMutablePointer<Void>!) -> CFComparisonResult)!
  init()
  init(version version: CFIndex, retain retain: (@convention(c) (CFAllocator!, UnsafePointer<Void>!) -> UnsafePointer<Void>!)!, release release: (@convention(c) (CFAllocator!, UnsafePointer<Void>!) -> Void)!, copyDescription copyDescription: (@convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!)!, compare compare: (@convention(c) (UnsafePointer<Void>!, UnsafePointer<Void>!, UnsafeMutablePointer<Void>!) -> CFComparisonResult)!)
}
let kCFStringBinaryHeapCallBacks: CFBinaryHeapCallBacks
typealias CFBinaryHeapApplierFunction = @convention(c) (UnsafePointer<Void>!, UnsafeMutablePointer<Void>!) -> Void
class CFBinaryHeap {
}
@discardableResult
func CFBinaryHeapGetTypeID() -> CFTypeID
@discardableResult
func CFBinaryHeapCreate(_ allocator: CFAllocator!, _ capacity: CFIndex, _ callBacks: UnsafePointer<CFBinaryHeapCallBacks>!, _ compareContext: UnsafePointer<CFBinaryHeapCompareContext>!) -> CFBinaryHeap!
@discardableResult
func CFBinaryHeapCreateCopy(_ allocator: CFAllocator!, _ capacity: CFIndex, _ heap: CFBinaryHeap!) -> CFBinaryHeap!
@discardableResult
func CFBinaryHeapGetCount(_ heap: CFBinaryHeap!) -> CFIndex
@discardableResult
func CFBinaryHeapGetCountOfValue(_ heap: CFBinaryHeap!, _ value: UnsafePointer<Void>!) -> CFIndex
@discardableResult
func CFBinaryHeapContainsValue(_ heap: CFBinaryHeap!, _ value: UnsafePointer<Void>!) -> Bool
@discardableResult
func CFBinaryHeapGetMinimum(_ heap: CFBinaryHeap!) -> UnsafePointer<Void>!
@discardableResult
func CFBinaryHeapGetMinimumIfPresent(_ heap: CFBinaryHeap!, _ value: UnsafeMutablePointer<UnsafePointer<Void>?>!) -> Bool
func CFBinaryHeapGetValues(_ heap: CFBinaryHeap!, _ values: UnsafeMutablePointer<UnsafePointer<Void>?>!)
func CFBinaryHeapApplyFunction(_ heap: CFBinaryHeap!, _ applier: CFBinaryHeapApplierFunction!, _ context: UnsafeMutablePointer<Void>!)
func CFBinaryHeapAddValue(_ heap: CFBinaryHeap!, _ value: UnsafePointer<Void>!)
func CFBinaryHeapRemoveMinimumValue(_ heap: CFBinaryHeap!)
func CFBinaryHeapRemoveAllValues(_ heap: CFBinaryHeap!)
