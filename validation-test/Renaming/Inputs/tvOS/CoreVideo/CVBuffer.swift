
@available(tvOS 4.0, *)
let kCVBufferPropagatedAttachmentsKey: CFString
@available(tvOS 4.0, *)
let kCVBufferNonPropagatedAttachmentsKey: CFString
@available(tvOS 4.0, *)
let kCVBufferMovieTimeKey: CFString
@available(tvOS 4.0, *)
let kCVBufferTimeValueKey: CFString
@available(tvOS 4.0, *)
let kCVBufferTimeScaleKey: CFString
typealias CVAttachmentMode = UInt32
var kCVAttachmentMode_ShouldNotPropagate: CVAttachmentMode { get }
var kCVAttachmentMode_ShouldPropagate: CVAttachmentMode { get }
class CVBuffer {
}
@available(tvOS 4.0, *)
func CVBufferSetAttachment(_ buffer: CVBuffer, _ key: CFString, _ value: CFTypeRef, _ attachmentMode: CVAttachmentMode)
@available(tvOS 4.0, *)
@discardableResult
func CVBufferGetAttachment(_ buffer: CVBuffer, _ key: CFString, _ attachmentMode: UnsafeMutablePointer<CVAttachmentMode>?) -> Unmanaged<CFTypeRef>?
@available(tvOS 4.0, *)
func CVBufferRemoveAttachment(_ buffer: CVBuffer, _ key: CFString)
@available(tvOS 4.0, *)
func CVBufferRemoveAllAttachments(_ buffer: CVBuffer)
@available(tvOS 4.0, *)
@discardableResult
func CVBufferGetAttachments(_ buffer: CVBuffer, _ attachmentMode: CVAttachmentMode) -> Unmanaged<CFDictionary>?
@available(tvOS 4.0, *)
func CVBufferSetAttachments(_ buffer: CVBuffer, _ theAttachments: CFDictionary, _ attachmentMode: CVAttachmentMode)
@available(tvOS 4.0, *)
func CVBufferPropagateAttachments(_ sourceBuffer: CVBuffer, _ destinationBuffer: CVBuffer)
