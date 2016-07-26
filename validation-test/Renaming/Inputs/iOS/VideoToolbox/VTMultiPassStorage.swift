
class VTMultiPassStorage {
}
@available(iOS 8.0, *)
@discardableResult
func VTMultiPassStorageGetTypeID() -> CFTypeID
@available(iOS 8.0, *)
@discardableResult
func VTMultiPassStorageCreate(_ allocator: CFAllocator?, _ fileURL: CFURL?, _ timeRange: CMTimeRange, _ options: CFDictionary?, _ multiPassStorageOut: UnsafeMutablePointer<VTMultiPassStorage?>) -> OSStatus
let kVTMultiPassStorageCreationOption_DoNotDelete: CFString
@available(iOS 8.0, *)
@discardableResult
func VTMultiPassStorageClose(_ multiPassStorage: VTMultiPassStorage) -> OSStatus
