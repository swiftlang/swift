
@available(OSX 10.4, *)
let kCVBufferPropagatedAttachmentsKey: CFString
@available(OSX 10.4, *)
let kCVBufferNonPropagatedAttachmentsKey: CFString
@available(OSX 10.4, *)
let kCVBufferMovieTimeKey: CFString
@available(OSX 10.4, *)
let kCVBufferTimeValueKey: CFString
@available(OSX 10.4, *)
let kCVBufferTimeScaleKey: CFString
typealias CVAttachmentMode = UInt32
var kCVAttachmentMode_ShouldNotPropagate: CVAttachmentMode { get }
var kCVAttachmentMode_ShouldPropagate: CVAttachmentMode { get }
class CVBuffer {
}
@available(OSX 10.4, *)
func CVBufferSetAttachment(_ buffer: CVBuffer, _ key: CFString, _ value: CFTypeRef, _ attachmentMode: CVAttachmentMode)
@available(OSX 10.4, *)
@discardableResult
func CVBufferGetAttachment(_ buffer: CVBuffer, _ key: CFString, _ attachmentMode: UnsafeMutablePointer<CVAttachmentMode>?) -> Unmanaged<CFTypeRef>?
@available(OSX 10.4, *)
func CVBufferRemoveAttachment(_ buffer: CVBuffer, _ key: CFString)
@available(OSX 10.4, *)
func CVBufferRemoveAllAttachments(_ buffer: CVBuffer)
@available(OSX 10.4, *)
@discardableResult
func CVBufferGetAttachments(_ buffer: CVBuffer, _ attachmentMode: CVAttachmentMode) -> Unmanaged<CFDictionary>?
@available(OSX 10.4, *)
func CVBufferSetAttachments(_ buffer: CVBuffer, _ theAttachments: CFDictionary, _ attachmentMode: CVAttachmentMode)
@available(OSX 10.4, *)
func CVBufferPropagateAttachments(_ sourceBuffer: CVBuffer, _ destinationBuffer: CVBuffer)
