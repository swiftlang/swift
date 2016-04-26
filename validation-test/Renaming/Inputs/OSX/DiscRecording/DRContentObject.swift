
class DRFileRef {
}
class DRFolderRef {
}
typealias DRFSObjectRef = DRType
typealias DRFilesystemMask = UInt32
var kDRFilesystemMaskISO9660: UInt32 { get }
var kDRFilesystemMaskJoliet: UInt32 { get }
var kDRFilesystemMaskUDF: UInt32 { get }
var kDRFilesystemMaskHFSPlus: UInt32 { get }
var kDRFilesystemMaskDefault: UInt32 { get }
@available(OSX 10.2, *)
@discardableResult
func DRFSObjectIsVirtual(_ object: DRFSObjectRef!) -> Bool
@available(OSX 10.2, *)
func DRFSObjectGetRealFSRef(_ object: DRFSObjectRef!, _ fsRef: UnsafeMutablePointer<FSRef>!)
@available(OSX 10.2, *)
@discardableResult
func DRFSObjectCopyRealURL(_ object: DRFSObjectRef!) -> Unmanaged<CFURL>!
@available(OSX 10.2, *)
@discardableResult
func DRFSObjectGetParent(_ object: DRFSObjectRef!) -> Unmanaged<DRFolderRef>!
@available(OSX 10.2, *)
@discardableResult
func DRFSObjectCopyBaseName(_ object: DRFSObjectRef!) -> Unmanaged<CFString>!
@available(OSX 10.2, *)
@discardableResult
func DRFSObjectCopySpecificName(_ object: DRFSObjectRef!, _ fsKey: CFString!) -> Unmanaged<CFString>!
@available(OSX 10.2, *)
@discardableResult
func DRFSObjectCopySpecificNames(_ object: DRFSObjectRef!) -> Unmanaged<CFDictionary>!
@available(OSX 10.2, *)
@discardableResult
func DRFSObjectCopyMangledName(_ object: DRFSObjectRef!, _ fsKey: CFString!) -> Unmanaged<CFString>!
@available(OSX 10.2, *)
@discardableResult
func DRFSObjectCopyMangledNames(_ object: DRFSObjectRef!) -> Unmanaged<CFDictionary>!
@available(OSX 10.2, *)
@discardableResult
func DRFSObjectCopyFilesystemProperty(_ object: DRFSObjectRef!, _ fsKey: CFString!, _ propertyKey: CFString!, _ coalesce: Bool) -> Unmanaged<CFTypeRef>!
@available(OSX 10.2, *)
@discardableResult
func DRFSObjectCopyFilesystemProperties(_ object: DRFSObjectRef!, _ fsKey: CFString!, _ coalesce: Bool) -> Unmanaged<CFDictionary>!
@available(OSX 10.2, *)
@discardableResult
func DRFSObjectGetFilesystemMask(_ object: DRFSObjectRef!, _ explicitMask: UnsafeMutablePointer<DRFilesystemMask>!, _ effectiveMask: UnsafeMutablePointer<DRFilesystemMask>!) -> DRFilesystemMask
@available(OSX 10.2, *)
func DRFSObjectSetBaseName(_ object: DRFSObjectRef!, _ baseName: CFString!)
@available(OSX 10.2, *)
func DRFSObjectSetSpecificName(_ object: DRFSObjectRef!, _ fsKey: CFString!, _ specificName: CFString!)
@available(OSX 10.2, *)
func DRFSObjectSetSpecificNames(_ object: DRFSObjectRef!, _ specificNames: CFDictionary!)
@available(OSX 10.2, *)
func DRFSObjectSetFilesystemProperty(_ object: DRFSObjectRef!, _ fsKey: CFString!, _ propertyKey: CFString!, _ value: CFTypeRef!)
@available(OSX 10.2, *)
func DRFSObjectSetFilesystemProperties(_ object: DRFSObjectRef!, _ fsKey: CFString!, _ properties: CFDictionary!)
@available(OSX 10.2, *)
func DRFSObjectSetFilesystemMask(_ object: DRFSObjectRef!, _ newMask: DRFilesystemMask)
