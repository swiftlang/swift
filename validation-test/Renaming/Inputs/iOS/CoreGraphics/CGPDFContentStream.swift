
typealias CGPDFContentStreamRef = OpaquePointer
@available(iOS 2.0, *)
@discardableResult
func CGPDFContentStreamCreateWithPage(_ page: CGPDFPage!) -> CGPDFContentStreamRef!
@available(iOS 2.0, *)
@discardableResult
func CGPDFContentStreamCreateWithStream(_ stream: CGPDFStreamRef!, _ streamResources: CGPDFDictionaryRef!, _ parent: CGPDFContentStreamRef!) -> CGPDFContentStreamRef!
@available(iOS 2.0, *)
@discardableResult
func CGPDFContentStreamRetain(_ cs: CGPDFContentStreamRef!) -> CGPDFContentStreamRef!
@available(iOS 2.0, *)
func CGPDFContentStreamRelease(_ cs: CGPDFContentStreamRef!)
@available(iOS 2.0, *)
@discardableResult
func CGPDFContentStreamGetStreams(_ cs: CGPDFContentStreamRef!) -> CFArray!
@available(iOS 2.0, *)
@discardableResult
func CGPDFContentStreamGetResource(_ cs: CGPDFContentStreamRef!, _ category: UnsafePointer<Int8>!, _ name: UnsafePointer<Int8>!) -> CGPDFObjectRef!
