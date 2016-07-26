
@available(OSX 10.4, *)
let kCVOpenGLBufferWidth: CFString
@available(OSX 10.4, *)
let kCVOpenGLBufferHeight: CFString
@available(OSX 10.4, *)
let kCVOpenGLBufferTarget: CFString
@available(OSX 10.4, *)
let kCVOpenGLBufferInternalFormat: CFString
@available(OSX 10.4, *)
let kCVOpenGLBufferMaximumMipmapLevel: CFString
typealias CVOpenGLBuffer = CVImageBuffer
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLBufferGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLBufferCreate(_ allocator: CFAllocator?, _ width: Int, _ height: Int, _ attributes: CFDictionary?, _ bufferOut: UnsafeMutablePointer<CVOpenGLBuffer?>) -> CVReturn
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLBufferGetAttributes(_ openGLBuffer: CVOpenGLBuffer) -> Unmanaged<CFDictionary>?
@available(OSX 10.4, *)
@discardableResult
func CVOpenGLBufferAttach(_ openGLBuffer: CVOpenGLBuffer, _ cglContext: CGLContextObj, _ face: GLenum, _ level: GLint, _ screen: GLint) -> CVReturn
