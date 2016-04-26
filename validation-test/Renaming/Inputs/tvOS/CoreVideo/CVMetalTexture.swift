
typealias CVMetalTexture = CVImageBuffer
@available(tvOS 8.0, *)
@discardableResult
func CVMetalTextureGetTypeID() -> CFTypeID
@available(tvOS 8.0, *)
@discardableResult
func CVMetalTextureGetTexture(_ image: CVMetalTexture) -> MTLTexture?
@available(tvOS 8.0, *)
@discardableResult
func CVMetalTextureIsFlipped(_ image: CVMetalTexture) -> Bool
@available(tvOS 8.0, *)
func CVMetalTextureGetCleanTexCoords(_ image: CVMetalTexture, _ lowerLeft: UnsafeMutablePointer<Float>!, _ lowerRight: UnsafeMutablePointer<Float>!, _ upperRight: UnsafeMutablePointer<Float>!, _ upperLeft: UnsafeMutablePointer<Float>!)
