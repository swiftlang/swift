
var CL_CONTEXT_PROPERTY_USE_CGL_SHAREGROUP_APPLE: Int32 { get }
@available(OSX 10.6, *)
@discardableResult
func clGetGLContextInfoAPPLE(_ _: cl_context, _ _: UnsafeMutablePointer<Void>, _ _: cl_gl_platform_info, _ _: Int, _ _: UnsafeMutablePointer<Void>?, _ _: UnsafeMutablePointer<Int>?) -> cl_int
var CL_CGL_DEVICE_FOR_CURRENT_VIRTUAL_SCREEN_APPLE: Int32 { get }
var CL_CGL_DEVICES_FOR_SUPPORTED_VIRTUAL_SCREENS_APPLE: Int32 { get }
var CL_INVALID_GL_CONTEXT_APPLE: Int32 { get }
var CL_COMMAND_GL_FENCE_SYNC_OBJECT_KHR: Int32 { get }
@available(OSX 10.7, *)
@discardableResult
func clCreateEventFromGLsyncKHR(_ _: cl_context, _ _: cl_GLsync, _ _: UnsafeMutablePointer<cl_int>?) -> cl_event?
@available(OSX 10.7, *)
@discardableResult
func clCreateImageFromIOSurface2DAPPLE(_ _: cl_context, _ _: cl_mem_flags, _ _: UnsafePointer<cl_image_format>, _ _: Int, _ _: Int, _ _: IOSurface, _ _: UnsafeMutablePointer<cl_int>?) -> cl_mem?
typealias cl_iosurface_properties_APPLE = Int
var CL_IOSURFACE_REF_APPLE: Int32 { get }
var CL_IOSURFACE_PLANE_APPLE: Int32 { get }
@available(OSX 10.8, *)
@discardableResult
func clCreateImageFromIOSurfaceWithPropertiesAPPLE(_ _: cl_context, _ _: cl_mem_flags, _ _: UnsafePointer<cl_image_format>, _ _: UnsafePointer<cl_image_desc>, _ _: UnsafeMutablePointer<cl_iosurface_properties_APPLE>, _ _: UnsafeMutablePointer<cl_int>?) -> cl_mem?
var CL_IMAGE_IOSURFACE_APPLE: Int32 { get }
var CL_IMAGE_IOSURFACE_PLANE_APPLE: Int32 { get }
