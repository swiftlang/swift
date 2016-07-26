
typealias CFArrayRetainCallBack = @convention(c) (CFAllocator!, UnsafePointer<Void>!) -> UnsafePointer<Void>!
typealias CFArrayReleaseCallBack = @convention(c) (CFAllocator!, UnsafePointer<Void>!) -> Void
typealias CFArrayCopyDescriptionCallBack = @convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!
typealias CFArrayEqualCallBack = @convention(c) (UnsafePointer<Void>!, UnsafePointer<Void>!) -> DarwinBoolean
struct CFArrayCallBacks {
  var version: CFIndex
  var retain: CFArrayRetainCallBack!
  var release: CFArrayReleaseCallBack!
  var copyDescription: CFArrayCopyDescriptionCallBack!
  var equal: CFArrayEqualCallBack!
  init()
  init(version version: CFIndex, retain retain: CFArrayRetainCallBack!, release release: CFArrayReleaseCallBack!, copyDescription copyDescription: CFArrayCopyDescriptionCallBack!, equal equal: CFArrayEqualCallBack!)
}
let kCFTypeArrayCallBacks: CFArrayCallBacks
typealias CFArrayApplierFunction = @convention(c) (UnsafePointer<Void>!, UnsafeMutablePointer<Void>!) -> Void
class CFArray {
}
class CFMutableArray {
}
@discardableResult
func CFArrayGetTypeID() -> CFTypeID
@discardableResult
func CFArrayCreate(_ allocator: CFAllocator!, _ values: UnsafeMutablePointer<UnsafePointer<Void>?>!, _ numValues: CFIndex, _ callBacks: UnsafePointer<CFArrayCallBacks>!) -> CFArray!
@discardableResult
func CFArrayCreateCopy(_ allocator: CFAllocator!, _ theArray: CFArray!) -> CFArray!
@discardableResult
func CFArrayCreateMutable(_ allocator: CFAllocator!, _ capacity: CFIndex, _ callBacks: UnsafePointer<CFArrayCallBacks>!) -> CFMutableArray!
@discardableResult
func CFArrayCreateMutableCopy(_ allocator: CFAllocator!, _ capacity: CFIndex, _ theArray: CFArray!) -> CFMutableArray!
@discardableResult
func CFArrayGetCount(_ theArray: CFArray!) -> CFIndex
@discardableResult
func CFArrayGetCountOfValue(_ theArray: CFArray!, _ range: CFRange, _ value: UnsafePointer<Void>!) -> CFIndex
@discardableResult
func CFArrayContainsValue(_ theArray: CFArray!, _ range: CFRange, _ value: UnsafePointer<Void>!) -> Bool
@discardableResult
func CFArrayGetValueAtIndex(_ theArray: CFArray!, _ idx: CFIndex) -> UnsafePointer<Void>!
func CFArrayGetValues(_ theArray: CFArray!, _ range: CFRange, _ values: UnsafeMutablePointer<UnsafePointer<Void>?>!)
func CFArrayApplyFunction(_ theArray: CFArray!, _ range: CFRange, _ applier: CFArrayApplierFunction!, _ context: UnsafeMutablePointer<Void>!)
@discardableResult
func CFArrayGetFirstIndexOfValue(_ theArray: CFArray!, _ range: CFRange, _ value: UnsafePointer<Void>!) -> CFIndex
@discardableResult
func CFArrayGetLastIndexOfValue(_ theArray: CFArray!, _ range: CFRange, _ value: UnsafePointer<Void>!) -> CFIndex
@discardableResult
func CFArrayBSearchValues(_ theArray: CFArray!, _ range: CFRange, _ value: UnsafePointer<Void>!, _ comparator: CFComparatorFunction!, _ context: UnsafeMutablePointer<Void>!) -> CFIndex
func CFArrayAppendValue(_ theArray: CFMutableArray!, _ value: UnsafePointer<Void>!)
func CFArrayInsertValueAtIndex(_ theArray: CFMutableArray!, _ idx: CFIndex, _ value: UnsafePointer<Void>!)
func CFArraySetValueAtIndex(_ theArray: CFMutableArray!, _ idx: CFIndex, _ value: UnsafePointer<Void>!)
func CFArrayRemoveValueAtIndex(_ theArray: CFMutableArray!, _ idx: CFIndex)
func CFArrayRemoveAllValues(_ theArray: CFMutableArray!)
func CFArrayReplaceValues(_ theArray: CFMutableArray!, _ range: CFRange, _ newValues: UnsafeMutablePointer<UnsafePointer<Void>?>!, _ newCount: CFIndex)
func CFArrayExchangeValuesAtIndices(_ theArray: CFMutableArray!, _ idx1: CFIndex, _ idx2: CFIndex)
func CFArraySortValues(_ theArray: CFMutableArray!, _ range: CFRange, _ comparator: CFComparatorFunction!, _ context: UnsafeMutablePointer<Void>!)
func CFArrayAppendArray(_ theArray: CFMutableArray!, _ otherArray: CFArray!, _ otherRange: CFRange)
