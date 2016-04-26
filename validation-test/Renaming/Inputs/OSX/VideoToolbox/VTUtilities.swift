
@available(OSX 10.11, *)
@discardableResult
func VTCreateCGImageFromCVPixelBuffer(_ pixelBuffer: CVPixelBuffer, _ options: CFDictionary?, _ imageOut: UnsafeMutablePointer<CGImage?>) -> OSStatus
