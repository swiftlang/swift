
typealias CFDictionaryRetainCallBack = @convention(c) (CFAllocator!, UnsafePointer<Void>!) -> UnsafePointer<Void>!
typealias CFDictionaryReleaseCallBack = @convention(c) (CFAllocator!, UnsafePointer<Void>!) -> Void
typealias CFDictionaryCopyDescriptionCallBack = @convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!
typealias CFDictionaryEqualCallBack = @convention(c) (UnsafePointer<Void>!, UnsafePointer<Void>!) -> DarwinBoolean
typealias CFDictionaryHashCallBack = @convention(c) (UnsafePointer<Void>!) -> CFHashCode
struct CFDictionaryKeyCallBacks {
  var version: CFIndex
  var retain: CFDictionaryRetainCallBack!
  var release: CFDictionaryReleaseCallBack!
  var copyDescription: CFDictionaryCopyDescriptionCallBack!
  var equal: CFDictionaryEqualCallBack!
  var hash: CFDictionaryHashCallBack!
  init()
  init(version version: CFIndex, retain retain: CFDictionaryRetainCallBack!, release release: CFDictionaryReleaseCallBack!, copyDescription copyDescription: CFDictionaryCopyDescriptionCallBack!, equal equal: CFDictionaryEqualCallBack!, hash hash: CFDictionaryHashCallBack!)
}
let kCFTypeDictionaryKeyCallBacks: CFDictionaryKeyCallBacks
let kCFCopyStringDictionaryKeyCallBacks: CFDictionaryKeyCallBacks
struct CFDictionaryValueCallBacks {
  var version: CFIndex
  var retain: CFDictionaryRetainCallBack!
  var release: CFDictionaryReleaseCallBack!
  var copyDescription: CFDictionaryCopyDescriptionCallBack!
  var equal: CFDictionaryEqualCallBack!
  init()
  init(version version: CFIndex, retain retain: CFDictionaryRetainCallBack!, release release: CFDictionaryReleaseCallBack!, copyDescription copyDescription: CFDictionaryCopyDescriptionCallBack!, equal equal: CFDictionaryEqualCallBack!)
}
let kCFTypeDictionaryValueCallBacks: CFDictionaryValueCallBacks
typealias CFDictionaryApplierFunction = @convention(c) (UnsafePointer<Void>!, UnsafePointer<Void>!, UnsafeMutablePointer<Void>!) -> Void
class CFDictionary {
}
class CFMutableDictionary {
}
@discardableResult
func CFDictionaryGetTypeID() -> CFTypeID
@discardableResult
func CFDictionaryCreate(_ allocator: CFAllocator!, _ keys: UnsafeMutablePointer<UnsafePointer<Void>?>!, _ values: UnsafeMutablePointer<UnsafePointer<Void>?>!, _ numValues: CFIndex, _ keyCallBacks: UnsafePointer<CFDictionaryKeyCallBacks>!, _ valueCallBacks: UnsafePointer<CFDictionaryValueCallBacks>!) -> CFDictionary!
@discardableResult
func CFDictionaryCreateCopy(_ allocator: CFAllocator!, _ theDict: CFDictionary!) -> CFDictionary!
@discardableResult
func CFDictionaryCreateMutable(_ allocator: CFAllocator!, _ capacity: CFIndex, _ keyCallBacks: UnsafePointer<CFDictionaryKeyCallBacks>!, _ valueCallBacks: UnsafePointer<CFDictionaryValueCallBacks>!) -> CFMutableDictionary!
@discardableResult
func CFDictionaryCreateMutableCopy(_ allocator: CFAllocator!, _ capacity: CFIndex, _ theDict: CFDictionary!) -> CFMutableDictionary!
@discardableResult
func CFDictionaryGetCount(_ theDict: CFDictionary!) -> CFIndex
@discardableResult
func CFDictionaryGetCountOfKey(_ theDict: CFDictionary!, _ key: UnsafePointer<Void>!) -> CFIndex
@discardableResult
func CFDictionaryGetCountOfValue(_ theDict: CFDictionary!, _ value: UnsafePointer<Void>!) -> CFIndex
@discardableResult
func CFDictionaryContainsKey(_ theDict: CFDictionary!, _ key: UnsafePointer<Void>!) -> Bool
@discardableResult
func CFDictionaryContainsValue(_ theDict: CFDictionary!, _ value: UnsafePointer<Void>!) -> Bool
@discardableResult
func CFDictionaryGetValue(_ theDict: CFDictionary!, _ key: UnsafePointer<Void>!) -> UnsafePointer<Void>!
@discardableResult
func CFDictionaryGetValueIfPresent(_ theDict: CFDictionary!, _ key: UnsafePointer<Void>!, _ value: UnsafeMutablePointer<UnsafePointer<Void>?>!) -> Bool
func CFDictionaryGetKeysAndValues(_ theDict: CFDictionary!, _ keys: UnsafeMutablePointer<UnsafePointer<Void>?>!, _ values: UnsafeMutablePointer<UnsafePointer<Void>?>!)
func CFDictionaryApplyFunction(_ theDict: CFDictionary!, _ applier: CFDictionaryApplierFunction!, _ context: UnsafeMutablePointer<Void>!)
func CFDictionaryAddValue(_ theDict: CFMutableDictionary!, _ key: UnsafePointer<Void>!, _ value: UnsafePointer<Void>!)
func CFDictionarySetValue(_ theDict: CFMutableDictionary!, _ key: UnsafePointer<Void>!, _ value: UnsafePointer<Void>!)
func CFDictionaryReplaceValue(_ theDict: CFMutableDictionary!, _ key: UnsafePointer<Void>!, _ value: UnsafePointer<Void>!)
func CFDictionaryRemoveValue(_ theDict: CFMutableDictionary!, _ key: UnsafePointer<Void>!)
func CFDictionaryRemoveAllValues(_ theDict: CFMutableDictionary!)
