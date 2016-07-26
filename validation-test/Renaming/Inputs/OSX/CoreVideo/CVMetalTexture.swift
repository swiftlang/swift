
typealias CVMetalTexture = CVImageBuffer
@available(OSX 10.11, *)
@discardableResult
func CVMetalTextureGetTypeID() -> CFTypeID
@available(OSX 10.11, *)
@discardableResult
func CVMetalTextureGetTexture(_ image: CVMetalTexture) -> MTLTexture?
@available(OSX 10.11, *)
@discardableResult
func CVMetalTextureIsFlipped(_ image: CVMetalTexture) -> Bool
@available(OSX 10.11, *)
func CVMetalTextureGetCleanTexCoords(_ image: CVMetalTexture, _ lowerLeft: UnsafeMutablePointer<Float>!, _ lowerRight: UnsafeMutablePointer<Float>!, _ upperRight: UnsafeMutablePointer<Float>!, _ upperLeft: UnsafeMutablePointer<Float>!)
