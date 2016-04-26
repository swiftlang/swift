
typealias CMAttachmentBearer = CFTypeRef
typealias CMAttachmentMode = UInt32
var kCMAttachmentMode_ShouldNotPropagate: CMAttachmentMode { get }
var kCMAttachmentMode_ShouldPropagate: CMAttachmentMode { get }
@available(iOS 4.0, *)
func CMSetAttachment(_ target: CMAttachmentBearer, _ key: CFString, _ value: CFTypeRef?, _ attachmentMode: CMAttachmentMode)
@available(iOS 4.0, *)
@discardableResult
func CMGetAttachment(_ target: CMAttachmentBearer, _ key: CFString, _ attachmentModeOut: UnsafeMutablePointer<CMAttachmentMode>?) -> CFTypeRef?
@available(iOS 4.0, *)
func CMRemoveAttachment(_ target: CMAttachmentBearer, _ key: CFString)
@available(iOS 4.0, *)
func CMRemoveAllAttachments(_ target: CMAttachmentBearer)
@available(iOS 4.0, *)
@discardableResult
func CMCopyDictionaryOfAttachments(_ allocator: CFAllocator?, _ target: CMAttachmentBearer, _ attachmentMode: CMAttachmentMode) -> CFDictionary?
@available(iOS 4.0, *)
func CMSetAttachments(_ target: CMAttachmentBearer, _ theAttachments: CFDictionary, _ attachmentMode: CMAttachmentMode)
@available(iOS 4.0, *)
func CMPropagateAttachments(_ source: CMAttachmentBearer, _ destination: CMAttachmentBearer)
