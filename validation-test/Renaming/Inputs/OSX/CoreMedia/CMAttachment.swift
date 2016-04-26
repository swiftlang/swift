
typealias CMAttachmentBearer = CFTypeRef
typealias CMAttachmentMode = UInt32
var kCMAttachmentMode_ShouldNotPropagate: CMAttachmentMode { get }
var kCMAttachmentMode_ShouldPropagate: CMAttachmentMode { get }
@available(OSX 10.7, *)
func CMSetAttachment(_ target: CMAttachmentBearer, _ key: CFString, _ value: CFTypeRef?, _ attachmentMode: CMAttachmentMode)
@available(OSX 10.7, *)
@discardableResult
func CMGetAttachment(_ target: CMAttachmentBearer, _ key: CFString, _ attachmentModeOut: UnsafeMutablePointer<CMAttachmentMode>?) -> CFTypeRef?
@available(OSX 10.7, *)
func CMRemoveAttachment(_ target: CMAttachmentBearer, _ key: CFString)
@available(OSX 10.7, *)
func CMRemoveAllAttachments(_ target: CMAttachmentBearer)
@available(OSX 10.7, *)
@discardableResult
func CMCopyDictionaryOfAttachments(_ allocator: CFAllocator?, _ target: CMAttachmentBearer, _ attachmentMode: CMAttachmentMode) -> CFDictionary?
@available(OSX 10.7, *)
func CMSetAttachments(_ target: CMAttachmentBearer, _ theAttachments: CFDictionary, _ attachmentMode: CMAttachmentMode)
@available(OSX 10.7, *)
func CMPropagateAttachments(_ source: CMAttachmentBearer, _ destination: CMAttachmentBearer)
