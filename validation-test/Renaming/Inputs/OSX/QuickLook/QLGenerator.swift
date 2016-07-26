
var kQLReturnMask: Int32 { get }
class QLThumbnailRequest {
}
@discardableResult
func QLThumbnailRequestGetTypeID() -> CFTypeID
@discardableResult
func QLThumbnailRequestCopyURL(_ thumbnail: QLThumbnailRequest!) -> Unmanaged<CFURL>!
@discardableResult
func QLThumbnailRequestCopyOptions(_ thumbnail: QLThumbnailRequest!) -> Unmanaged<CFDictionary>!
@discardableResult
func QLThumbnailRequestCopyContentUTI(_ thumbnail: QLThumbnailRequest!) -> Unmanaged<CFString>!
@discardableResult
func QLThumbnailRequestGetMaximumSize(_ thumbnail: QLThumbnailRequest!) -> CGSize
@discardableResult
func QLThumbnailRequestGetGeneratorBundle(_ thumbnail: QLThumbnailRequest!) -> Unmanaged<CFBundle>!
@available(OSX 10.6, *)
func QLThumbnailRequestSetDocumentObject(_ thumbnail: QLThumbnailRequest!, _ object: UnsafePointer<Void>!, _ callbacks: UnsafePointer<CFArrayCallBacks>!)
@available(OSX 10.6, *)
@discardableResult
func QLThumbnailRequestGetDocumentObject(_ thumbnail: QLThumbnailRequest!) -> UnsafePointer<Void>!
func QLThumbnailRequestSetImage(_ thumbnail: QLThumbnailRequest!, _ image: CGImage!, _ properties: CFDictionary!)
func QLThumbnailRequestSetImageWithData(_ thumbnail: QLThumbnailRequest!, _ data: CFData!, _ properties: CFDictionary!)
@discardableResult
func QLThumbnailRequestCreateContext(_ thumbnail: QLThumbnailRequest!, _ size: CGSize, _ isBitmap: Bool, _ properties: CFDictionary!) -> Unmanaged<CGContext>!
func QLThumbnailRequestFlushContext(_ thumbnail: QLThumbnailRequest!, _ context: CGContext!)
func QLThumbnailRequestSetImageAtURL(_ thumbnail: QLThumbnailRequest!, _ url: CFURL!, _ properties: CFDictionary!)
@available(OSX 10.6, *)
func QLThumbnailRequestSetThumbnailWithDataRepresentation(_ thumbnail: QLThumbnailRequest!, _ data: CFData!, _ contentTypeUTI: CFString!, _ previewProperties: CFDictionary!, _ properties: CFDictionary!)
@available(OSX 10.6, *)
func QLThumbnailRequestSetThumbnailWithURLRepresentation(_ thumbnail: QLThumbnailRequest!, _ url: CFURL!, _ contentTypeUTI: CFString!, _ previewProperties: CFDictionary!, _ properties: CFDictionary!)
@discardableResult
func QLThumbnailRequestIsCancelled(_ thumbnail: QLThumbnailRequest!) -> Bool
@available(OSX 10.6, *)
let kQLThumbnailPropertyExtensionKey: CFString!
@available(OSX 10.6, *)
let kQLThumbnailPropertyBadgeImageKey: CFString!
@available(OSX 10.6, *)
let kQLThumbnailPropertyBaseBundlePathKey: CFString!
class QLPreviewRequest {
}
@discardableResult
func QLPreviewRequestGetTypeID() -> CFTypeID
let kQLPreviewPropertyDisplayNameKey: CFString!
let kQLPreviewPropertyWidthKey: CFString!
let kQLPreviewPropertyHeightKey: CFString!
@available(OSX 10.6, *)
let kQLPreviewPropertyBaseBundlePathKey: CFString!
let kQLPreviewPropertyStringEncodingKey: CFString!
struct QLPreviewPDFStyle : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kQLPreviewPDFStandardStyle: QLPreviewPDFStyle { get }
var kQLPreviewPDFPagesWithThumbnailsOnRightStyle: QLPreviewPDFStyle { get }
var kQLPreviewPDFPagesWithThumbnailsOnLeftStyle: QLPreviewPDFStyle { get }
@available(OSX 10.6, *)
let kQLPreviewPropertyPDFStyleKey: CFString!
@available(OSX 10.6, *)
let kQLPreviewOptionCursorKey: CFString!
@available(OSX 10.6, *)
let kQLPreviewPropertyCursorKey: CFString!
@discardableResult
func QLPreviewRequestCopyURL(_ preview: QLPreviewRequest!) -> Unmanaged<CFURL>!
@discardableResult
func QLPreviewRequestCopyOptions(_ preview: QLPreviewRequest!) -> Unmanaged<CFDictionary>!
@discardableResult
func QLPreviewRequestCopyContentUTI(_ preview: QLPreviewRequest!) -> Unmanaged<CFString>!
@discardableResult
func QLPreviewRequestGetGeneratorBundle(_ preview: QLPreviewRequest!) -> Unmanaged<CFBundle>!
@available(OSX 10.6, *)
func QLPreviewRequestSetDocumentObject(_ preview: QLPreviewRequest!, _ object: UnsafePointer<Void>!, _ callbacks: UnsafePointer<CFArrayCallBacks>!)
@available(OSX 10.6, *)
@discardableResult
func QLPreviewRequestGetDocumentObject(_ preview: QLPreviewRequest!) -> UnsafePointer<Void>!
@discardableResult
func QLPreviewRequestIsCancelled(_ preview: QLPreviewRequest!) -> Bool
func QLPreviewRequestSetDataRepresentation(_ preview: QLPreviewRequest!, _ data: CFData!, _ contentTypeUTI: CFString!, _ properties: CFDictionary!)
func QLPreviewRequestSetURLRepresentation(_ preview: QLPreviewRequest!, _ url: CFURL!, _ contentTypeUTI: CFString!, _ properties: CFDictionary!)
@discardableResult
func QLPreviewRequestCreateContext(_ preview: QLPreviewRequest!, _ size: CGSize, _ isBitmap: Bool, _ properties: CFDictionary!) -> Unmanaged<CGContext>!
@discardableResult
func QLPreviewRequestCreatePDFContext(_ preview: QLPreviewRequest!, _ mediaBox: UnsafePointer<CGRect>!, _ auxiliaryInfo: CFDictionary!, _ properties: CFDictionary!) -> Unmanaged<CGContext>!
func QLPreviewRequestFlushContext(_ preview: QLPreviewRequest!, _ context: CGContext!)
let kQLPreviewPropertyMIMETypeKey: CFString!
let kQLPreviewPropertyTextEncodingNameKey: CFString!
let kQLPreviewPropertyAttachmentDataKey: CFString!
let kQLPreviewPropertyAttachmentsKey: CFString!
let kQLPreviewContentIDScheme: CFString!
struct QLGeneratorInterfaceStruct {
  var _reserved: UnsafeMutablePointer<Void>!
  var QueryInterface: (@convention(c) (UnsafeMutablePointer<Void>!, REFIID, UnsafeMutablePointer<LPVOID?>!) -> HRESULT)!
  var AddRef: (@convention(c) (UnsafeMutablePointer<Void>!) -> ULONG)!
  var Release: (@convention(c) (UnsafeMutablePointer<Void>!) -> ULONG)!
  var GenerateThumbnailForURL: (@convention(c) (UnsafeMutablePointer<Void>!, QLThumbnailRequest!, CFURL!, CFString!, CFDictionary!, CGSize) -> OSStatus)!
  var CancelThumbnailGeneration: (@convention(c) (UnsafeMutablePointer<Void>!, QLThumbnailRequest!) -> Void)!
  var GeneratePreviewForURL: (@convention(c) (UnsafeMutablePointer<Void>!, QLPreviewRequest!, CFURL!, CFString!, CFDictionary!) -> OSStatus)!
  var CancelPreviewGeneration: (@convention(c) (UnsafeMutablePointer<Void>!, QLPreviewRequest!) -> Void)!
  init()
  init(_reserved _reserved: UnsafeMutablePointer<Void>!, QueryInterface QueryInterface: (@convention(c) (UnsafeMutablePointer<Void>!, REFIID, UnsafeMutablePointer<LPVOID?>!) -> HRESULT)!, AddRef AddRef: (@convention(c) (UnsafeMutablePointer<Void>!) -> ULONG)!, Release Release: (@convention(c) (UnsafeMutablePointer<Void>!) -> ULONG)!, GenerateThumbnailForURL GenerateThumbnailForURL: (@convention(c) (UnsafeMutablePointer<Void>!, QLThumbnailRequest!, CFURL!, CFString!, CFDictionary!, CGSize) -> OSStatus)!, CancelThumbnailGeneration CancelThumbnailGeneration: (@convention(c) (UnsafeMutablePointer<Void>!, QLThumbnailRequest!) -> Void)!, GeneratePreviewForURL GeneratePreviewForURL: (@convention(c) (UnsafeMutablePointer<Void>!, QLPreviewRequest!, CFURL!, CFString!, CFDictionary!) -> OSStatus)!, CancelPreviewGeneration CancelPreviewGeneration: (@convention(c) (UnsafeMutablePointer<Void>!, QLPreviewRequest!) -> Void)!)
}
