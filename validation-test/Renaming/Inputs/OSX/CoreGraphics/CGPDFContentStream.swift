
typealias CGPDFContentStreamRef = OpaquePointer
@available(OSX 10.4, *)
@discardableResult
func CGPDFContentStreamCreateWithPage(_ page: CGPDFPage!) -> CGPDFContentStreamRef!
@available(OSX 10.4, *)
@discardableResult
func CGPDFContentStreamCreateWithStream(_ stream: CGPDFStreamRef!, _ streamResources: CGPDFDictionaryRef!, _ parent: CGPDFContentStreamRef!) -> CGPDFContentStreamRef!
@available(OSX 10.4, *)
@discardableResult
func CGPDFContentStreamRetain(_ cs: CGPDFContentStreamRef!) -> CGPDFContentStreamRef!
@available(OSX 10.4, *)
func CGPDFContentStreamRelease(_ cs: CGPDFContentStreamRef!)
@available(OSX 10.4, *)
@discardableResult
func CGPDFContentStreamGetStreams(_ cs: CGPDFContentStreamRef!) -> CFArray!
@available(OSX 10.4, *)
@discardableResult
func CGPDFContentStreamGetResource(_ cs: CGPDFContentStreamRef!, _ category: UnsafePointer<Int8>!, _ name: UnsafePointer<Int8>!) -> CGPDFObjectRef!
