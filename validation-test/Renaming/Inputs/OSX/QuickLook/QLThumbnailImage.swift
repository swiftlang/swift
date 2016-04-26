
@available(OSX 10.5, *)
@discardableResult
func QLThumbnailImageCreate(_ allocator: CFAllocator!, _ url: CFURL!, _ maxThumbnailSize: CGSize, _ options: CFDictionary!) -> Unmanaged<CGImage>!
let kQLThumbnailOptionIconModeKey: CFString!
let kQLThumbnailOptionScaleFactorKey: CFString!
