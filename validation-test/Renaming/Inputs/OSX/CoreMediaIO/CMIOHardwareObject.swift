
typealias CMIOObjectPropertySelector = UInt32
typealias CMIOObjectPropertyScope = UInt32
typealias CMIOObjectPropertyElement = UInt32
struct CMIOObjectPropertyAddress {
  var mSelector: CMIOObjectPropertySelector
  var mScope: CMIOObjectPropertyScope
  var mElement: CMIOObjectPropertyElement
  init()
  init(mSelector mSelector: CMIOObjectPropertySelector, mScope mScope: CMIOObjectPropertyScope, mElement mElement: CMIOObjectPropertyElement)
}
var kCMIOObjectPropertySelectorWildcard: UInt32 { get }
var kCMIOObjectPropertyScopeWildcard: UInt32 { get }
var kCMIOObjectPropertyElementWildcard: UInt32 { get }
typealias CMIOClassID = UInt32
typealias CMIOObjectID = UInt32
typealias CMIOObjectPropertyListenerProc = @convention(c) (CMIOObjectID, UInt32, UnsafePointer<CMIOObjectPropertyAddress>!, UnsafeMutablePointer<Void>!) -> OSStatus
typealias CMIOObjectPropertyListenerBlock = (UInt32, UnsafePointer<CMIOObjectPropertyAddress>!) -> Void
var kCMIOObjectPropertyScopeGlobal: Int { get }
var kCMIOObjectPropertyElementMaster: Int { get }
var kCMIOObjectClassID: Int { get }
var kCMIOObjectClassIDWildcard: Int { get }
var kCMIOObjectUnknown: Int { get }
var kCMIOObjectPropertyClass: Int { get }
var kCMIOObjectPropertyOwner: Int { get }
var kCMIOObjectPropertyCreator: Int { get }
var kCMIOObjectPropertyName: Int { get }
var kCMIOObjectPropertyManufacturer: Int { get }
var kCMIOObjectPropertyElementName: Int { get }
var kCMIOObjectPropertyElementCategoryName: Int { get }
var kCMIOObjectPropertyElementNumberName: Int { get }
var kCMIOObjectPropertyOwnedObjects: Int { get }
var kCMIOObjectPropertyListenerAdded: Int { get }
var kCMIOObjectPropertyListenerRemoved: Int { get }
@available(OSX 10.7, *)
func CMIOObjectShow(_ objectID: CMIOObjectID)
@available(OSX 10.7, *)
@discardableResult
func CMIOObjectHasProperty(_ objectID: CMIOObjectID, _ address: UnsafePointer<CMIOObjectPropertyAddress>!) -> Bool
@available(OSX 10.7, *)
@discardableResult
func CMIOObjectIsPropertySettable(_ objectID: CMIOObjectID, _ address: UnsafePointer<CMIOObjectPropertyAddress>!, _ isSettable: UnsafeMutablePointer<DarwinBoolean>!) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOObjectGetPropertyDataSize(_ objectID: CMIOObjectID, _ address: UnsafePointer<CMIOObjectPropertyAddress>!, _ qualifierDataSize: UInt32, _ qualifierData: UnsafePointer<Void>!, _ dataSize: UnsafeMutablePointer<UInt32>!) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOObjectGetPropertyData(_ objectID: CMIOObjectID, _ address: UnsafePointer<CMIOObjectPropertyAddress>!, _ qualifierDataSize: UInt32, _ qualifierData: UnsafePointer<Void>!, _ dataSize: UInt32, _ dataUsed: UnsafeMutablePointer<UInt32>!, _ data: UnsafeMutablePointer<Void>!) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOObjectSetPropertyData(_ objectID: CMIOObjectID, _ address: UnsafePointer<CMIOObjectPropertyAddress>!, _ qualifierDataSize: UInt32, _ qualifierData: UnsafePointer<Void>!, _ dataSize: UInt32, _ data: UnsafePointer<Void>!) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOObjectAddPropertyListener(_ objectID: CMIOObjectID, _ address: UnsafePointer<CMIOObjectPropertyAddress>!, _ listener: CMIOObjectPropertyListenerProc!, _ clientData: UnsafeMutablePointer<Void>!) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIOObjectRemovePropertyListener(_ objectID: CMIOObjectID, _ address: UnsafePointer<CMIOObjectPropertyAddress>!, _ listener: CMIOObjectPropertyListenerProc!, _ clientData: UnsafeMutablePointer<Void>!) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func CMIOObjectAddPropertyListenerBlock(_ objectID: CMIOObjectID, _ address: UnsafePointer<CMIOObjectPropertyAddress>!, _ dispatchQueue: dispatch_queue_t!, _ listener: CMIOObjectPropertyListenerBlock!) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func CMIOObjectRemovePropertyListenerBlock(_ objectID: CMIOObjectID, _ address: UnsafePointer<CMIOObjectPropertyAddress>!, _ dispatchQueue: dispatch_queue_t!, _ listener: CMIOObjectPropertyListenerBlock!) -> OSStatus
