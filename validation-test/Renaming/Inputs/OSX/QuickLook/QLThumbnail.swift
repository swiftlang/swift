
class QLThumbnail {
}
@discardableResult
func QLThumbnailGetTypeID() -> CFTypeID
@available(OSX 10.6, *)
@discardableResult
func QLThumbnailCreate(_ allocator: CFAllocator!, _ url: CFURL!, _ maxThumbnailSize: CGSize, _ options: CFDictionary!) -> Unmanaged<QLThumbnail>!
@available(OSX 10.6, *)
@discardableResult
func QLThumbnailCopyDocumentURL(_ thumbnail: QLThumbnail!) -> Unmanaged<CFURL>!
@available(OSX 10.6, *)
@discardableResult
func QLThumbnailGetMaximumSize(_ thumbnail: QLThumbnail!) -> CGSize
@available(OSX 10.6, *)
@discardableResult
func QLThumbnailCopyOptions(_ thumbnail: QLThumbnail!) -> Unmanaged<CFDictionary>!
@available(OSX 10.6, *)
func QLThumbnailDispatchAsync(_ thumbnail: QLThumbnail!, _ queue: dispatch_queue_t!, _ completion: dispatch_block_t!)
@available(OSX 10.6, *)
@discardableResult
func QLThumbnailCopyImage(_ thumbnail: QLThumbnail!) -> Unmanaged<CGImage>!
@available(OSX 10.6, *)
@discardableResult
func QLThumbnailGetContentRect(_ thumbnail: QLThumbnail!) -> CGRect
@available(OSX 10.6, *)
func QLThumbnailCancel(_ thumbnail: QLThumbnail!)
@available(OSX 10.6, *)
@discardableResult
func QLThumbnailIsCancelled(_ thumbnail: QLThumbnail!) -> Bool
