
let kDADiskDescriptionVolumeKindKey: CFString
let kDADiskDescriptionVolumeMountableKey: CFString
let kDADiskDescriptionVolumeNameKey: CFString
let kDADiskDescriptionVolumeNetworkKey: CFString
let kDADiskDescriptionVolumePathKey: CFString
let kDADiskDescriptionVolumeTypeKey: CFString
let kDADiskDescriptionVolumeUUIDKey: CFString
let kDADiskDescriptionMediaBlockSizeKey: CFString
let kDADiskDescriptionMediaBSDMajorKey: CFString
let kDADiskDescriptionMediaBSDMinorKey: CFString
let kDADiskDescriptionMediaBSDNameKey: CFString
let kDADiskDescriptionMediaBSDUnitKey: CFString
let kDADiskDescriptionMediaContentKey: CFString
let kDADiskDescriptionMediaEjectableKey: CFString
let kDADiskDescriptionMediaIconKey: CFString
let kDADiskDescriptionMediaKindKey: CFString
let kDADiskDescriptionMediaLeafKey: CFString
let kDADiskDescriptionMediaNameKey: CFString
let kDADiskDescriptionMediaPathKey: CFString
let kDADiskDescriptionMediaRemovableKey: CFString
let kDADiskDescriptionMediaSizeKey: CFString
let kDADiskDescriptionMediaTypeKey: CFString
let kDADiskDescriptionMediaUUIDKey: CFString
let kDADiskDescriptionMediaWholeKey: CFString
let kDADiskDescriptionMediaWritableKey: CFString
let kDADiskDescriptionDeviceGUIDKey: CFString
let kDADiskDescriptionDeviceInternalKey: CFString
let kDADiskDescriptionDeviceModelKey: CFString
let kDADiskDescriptionDevicePathKey: CFString
let kDADiskDescriptionDeviceProtocolKey: CFString
let kDADiskDescriptionDeviceRevisionKey: CFString
let kDADiskDescriptionDeviceUnitKey: CFString
let kDADiskDescriptionDeviceVendorKey: CFString
let kDADiskDescriptionBusNameKey: CFString
let kDADiskDescriptionBusPathKey: CFString
class DADisk {
}
@discardableResult
func DADiskGetTypeID() -> CFTypeID
@discardableResult
func DADiskCreateFromBSDName(_ allocator: CFAllocator?, _ session: DASession, _ name: UnsafePointer<Int8>) -> DADisk?
@discardableResult
func DADiskCreateFromIOMedia(_ allocator: CFAllocator?, _ session: DASession, _ media: io_service_t) -> DADisk?
@discardableResult
func DADiskCreateFromVolumePath(_ allocator: CFAllocator?, _ session: DASession, _ path: CFURL) -> DADisk?
@discardableResult
func DADiskGetBSDName(_ disk: DADisk) -> UnsafePointer<Int8>?
@discardableResult
func DADiskCopyIOMedia(_ disk: DADisk) -> io_service_t
@discardableResult
func DADiskCopyDescription(_ disk: DADisk) -> CFDictionary?
@discardableResult
func DADiskCopyWholeDisk(_ disk: DADisk) -> DADisk?
