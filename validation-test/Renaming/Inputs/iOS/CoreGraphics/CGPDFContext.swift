
@available(iOS 2.0, *)
@discardableResult
func CGPDFContextCreate(_ consumer: CGDataConsumer?, _ mediaBox: UnsafePointer<CGRect>?, _ auxiliaryInfo: CFDictionary?) -> CGContext?
@available(iOS 2.0, *)
@discardableResult
func CGPDFContextCreateWithURL(_ url: CFURL?, _ mediaBox: UnsafePointer<CGRect>?, _ auxiliaryInfo: CFDictionary?) -> CGContext?
@available(iOS 2.0, *)
func CGPDFContextClose(_ context: CGContext?)
@available(iOS 2.0, *)
func CGPDFContextBeginPage(_ context: CGContext?, _ pageInfo: CFDictionary?)
@available(iOS 2.0, *)
func CGPDFContextEndPage(_ context: CGContext?)
@available(iOS 4.0, *)
func CGPDFContextAddDocumentMetadata(_ context: CGContext?, _ metadata: CFData?)
@available(iOS 2.0, *)
func CGPDFContextSetURLForRect(_ context: CGContext?, _ url: CFURL, _ rect: CGRect)
@available(iOS 2.0, *)
func CGPDFContextAddDestinationAtPoint(_ context: CGContext?, _ name: CFString, _ point: CGPoint)
@available(iOS 2.0, *)
func CGPDFContextSetDestinationForRect(_ context: CGContext?, _ name: CFString, _ rect: CGRect)
@available(iOS 2.0, *)
let kCGPDFContextMediaBox: CFString
@available(iOS 2.0, *)
let kCGPDFContextCropBox: CFString
@available(iOS 2.0, *)
let kCGPDFContextBleedBox: CFString
@available(iOS 2.0, *)
let kCGPDFContextTrimBox: CFString
@available(iOS 2.0, *)
let kCGPDFContextArtBox: CFString
@available(iOS 2.0, *)
let kCGPDFContextTitle: CFString
@available(iOS 2.0, *)
let kCGPDFContextAuthor: CFString
@available(iOS 2.0, *)
let kCGPDFContextSubject: CFString
@available(iOS 2.0, *)
let kCGPDFContextKeywords: CFString
@available(iOS 2.0, *)
let kCGPDFContextCreator: CFString
@available(iOS 2.0, *)
let kCGPDFContextOwnerPassword: CFString
@available(iOS 2.0, *)
let kCGPDFContextUserPassword: CFString
@available(iOS 2.0, *)
let kCGPDFContextEncryptionKeyLength: CFString
@available(iOS 2.0, *)
let kCGPDFContextAllowsPrinting: CFString
@available(iOS 2.0, *)
let kCGPDFContextAllowsCopying: CFString
