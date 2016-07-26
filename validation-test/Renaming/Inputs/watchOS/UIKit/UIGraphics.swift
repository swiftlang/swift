
@discardableResult
func UIGraphicsGetCurrentContext() -> CGContext?
func UIGraphicsPushContext(_ context: CGContext)
func UIGraphicsPopContext()
func UIRectFillUsingBlendMode(_ rect: CGRect, _ blendMode: CGBlendMode)
func UIRectFill(_ rect: CGRect)
func UIRectFrameUsingBlendMode(_ rect: CGRect, _ blendMode: CGBlendMode)
func UIRectFrame(_ rect: CGRect)
func UIRectClip(_ rect: CGRect)
func UIGraphicsBeginImageContext(_ size: CGSize)
@available(watchOS 2.0, *)
func UIGraphicsBeginImageContextWithOptions(_ size: CGSize, _ opaque: Bool, _ scale: CGFloat)
@discardableResult
func UIGraphicsGetImageFromCurrentImageContext() -> UIImage!
func UIGraphicsEndImageContext()
@available(watchOS 2.0, *)
@discardableResult
func UIGraphicsBeginPDFContextToFile(_ path: String, _ bounds: CGRect, _ documentInfo: [NSObject : AnyObject]?) -> Bool
@available(watchOS 2.0, *)
func UIGraphicsBeginPDFContextToData(_ data: NSMutableData, _ bounds: CGRect, _ documentInfo: [NSObject : AnyObject]?)
@available(watchOS 2.0, *)
func UIGraphicsEndPDFContext()
@available(watchOS 2.0, *)
func UIGraphicsBeginPDFPage()
@available(watchOS 2.0, *)
func UIGraphicsBeginPDFPageWithInfo(_ bounds: CGRect, _ pageInfo: [NSObject : AnyObject]?)
@available(watchOS 2.0, *)
@discardableResult
func UIGraphicsGetPDFContextBounds() -> CGRect
@available(watchOS 2.0, *)
func UIGraphicsSetPDFContextURLForRect(_ url: NSURL, _ rect: CGRect)
@available(watchOS 2.0, *)
func UIGraphicsAddPDFContextDestinationAtPoint(_ name: String, _ point: CGPoint)
@available(watchOS 2.0, *)
func UIGraphicsSetPDFContextDestinationForRect(_ name: String, _ rect: CGRect)
