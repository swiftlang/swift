
typealias cl_gl_object_type = cl_uint
typealias cl_gl_texture_info = cl_uint
typealias cl_gl_platform_info = cl_uint
typealias cl_GLsync = OpaquePointer
var CL_GL_OBJECT_BUFFER: Int32 { get }
var CL_GL_OBJECT_TEXTURE2D: Int32 { get }
var CL_GL_OBJECT_TEXTURE3D: Int32 { get }
var CL_GL_OBJECT_RENDERBUFFER: Int32 { get }
var CL_GL_OBJECT_TEXTURE2D_ARRAY: Int32 { get }
var CL_GL_OBJECT_TEXTURE1D: Int32 { get }
var CL_GL_OBJECT_TEXTURE1D_ARRAY: Int32 { get }
var CL_GL_OBJECT_TEXTURE_BUFFER: Int32 { get }
var CL_GL_TEXTURE_TARGET: Int32 { get }
var CL_GL_MIPMAP_LEVEL: Int32 { get }
var CL_GL_NUM_SAMPLES: Int32 { get }
@available(OSX 10.6, *)
@discardableResult
func clCreateFromGLBuffer(_ _: cl_context!, _ _: cl_mem_flags, _ _: cl_GLuint, _ _: UnsafeMutablePointer<Int32>!) -> cl_mem!
@available(OSX 10.8, *)
@discardableResult
func clCreateFromGLTexture(_ _: cl_context!, _ _: cl_mem_flags, _ _: cl_GLenum, _ _: cl_GLint, _ _: cl_GLuint, _ _: UnsafeMutablePointer<cl_int>!) -> cl_mem!
@available(OSX 10.6, *)
@discardableResult
func clCreateFromGLRenderbuffer(_ _: cl_context!, _ _: cl_mem_flags, _ _: cl_GLuint, _ _: UnsafeMutablePointer<cl_int>!) -> cl_mem!
@available(OSX 10.6, *)
@discardableResult
func clGetGLObjectInfo(_ _: cl_mem!, _ _: UnsafeMutablePointer<cl_gl_object_type>!, _ _: UnsafeMutablePointer<cl_GLuint>!) -> cl_int
@available(OSX 10.6, *)
@discardableResult
func clGetGLTextureInfo(_ _: cl_mem!, _ _: cl_gl_texture_info, _ _: Int, _ _: UnsafeMutablePointer<Void>!, _ _: UnsafeMutablePointer<Int>!) -> cl_int
@available(OSX 10.6, *)
@discardableResult
func clEnqueueAcquireGLObjects(_ _: cl_command_queue!, _ _: cl_uint, _ _: UnsafePointer<cl_mem?>!, _ _: cl_uint, _ _: UnsafePointer<cl_event?>!, _ _: UnsafeMutablePointer<cl_event?>!) -> cl_int
@available(OSX 10.6, *)
@discardableResult
func clEnqueueReleaseGLObjects(_ _: cl_command_queue!, _ _: cl_uint, _ _: UnsafePointer<cl_mem?>!, _ _: cl_uint, _ _: UnsafePointer<cl_event?>!, _ _: UnsafeMutablePointer<cl_event?>!) -> cl_int
