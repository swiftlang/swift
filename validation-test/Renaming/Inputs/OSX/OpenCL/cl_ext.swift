
var CL_DEVICE_DOUBLE_FP_CONFIG: Int32 { get }
var CL_DEVICE_HALF_FP_CONFIG: Int32 { get }
@available(OSX 10.6, *)
func clLogMessagesToSystemLogAPPLE(_ _: UnsafePointer<Int8>!, _ _: UnsafePointer<Void>!, _ _: Int, _ _: UnsafeMutablePointer<Void>!)
@available(OSX 10.6, *)
func clLogMessagesToStdoutAPPLE(_ _: UnsafePointer<Int8>!, _ _: UnsafePointer<Void>!, _ _: Int, _ _: UnsafeMutablePointer<Void>!)
@available(OSX 10.6, *)
func clLogMessagesToStderrAPPLE(_ _: UnsafePointer<Int8>!, _ _: UnsafePointer<Void>!, _ _: Int, _ _: UnsafeMutablePointer<Void>!)
@available(OSX 10.7, *)
@discardableResult
func clCreateContextAndCommandQueueAPPLE(_ _: UnsafePointer<cl_context_properties>!, _ _: cl_uint, _ _: UnsafePointer<cl_device_id?>!, _ _: (@convention(c) (UnsafePointer<Int8>!, UnsafePointer<Void>!, Int, UnsafeMutablePointer<Void>!) -> Void)!, _ _: UnsafeMutablePointer<Void>!, _ _: cl_command_queue_properties, _ _: UnsafeMutablePointer<cl_context?>!, _ _: UnsafeMutablePointer<cl_command_queue?>!) -> cl_int
@available(OSX 10.7, *)
@discardableResult
func clCreateProgramAndKernelsWithSourceAPPLE(_ _: cl_context!, _ _: cl_uint, _ _: UnsafeMutablePointer<UnsafePointer<Int8>?>!, _ _: UnsafePointer<Int>!, _ _: cl_uint, _ _: UnsafePointer<cl_device_id?>!, _ _: UnsafePointer<Int8>!, _ _: cl_uint, _ _: UnsafeMutablePointer<UnsafePointer<Int8>?>!, _ _: UnsafeMutablePointer<cl_program?>!, _ _: UnsafeMutablePointer<cl_kernel?>!) -> cl_int
@available(OSX 10.7, *)
@discardableResult
func clSetKernelArgsVaListAPPLE(_ _: cl_kernel!, _ _: cl_uint, _ _: CVaListPointer) -> cl_int
var CL_INVALID_ARG_NAME_APPLE: Int32 { get }
@available(OSX 10.7, *)
@discardableResult
func clSetKernelArgByNameAPPLE(_ _: cl_kernel!, _ _: UnsafePointer<Int8>!, _ _: Int, _ _: UnsafePointer<Void>!) -> cl_int
var CL_PROGRAM_NUM_KERNELS_APPLE: Int32 { get }
var CL_PROGRAM_KERNEL_NAMES_APPLE: Int32 { get }
var CL_1RGB_APPLE: Int32 { get }
var CL_BGR1_APPLE: Int32 { get }
var CL_SFIXED14_APPLE: Int32 { get }
var CL_BIASED_HALF_APPLE: Int32 { get }
var CL_YCbYCr_APPLE: Int32 { get }
var CL_CbYCrY_APPLE: Int32 { get }
var CL_ABGR_APPLE: Int32 { get }
typealias cl_dag = OpaquePointer
typealias cl_dag_node = Int32
@available(OSX 10.8, *)
@discardableResult
func clCreateDAGAPPLE(_ c: cl_context!) -> cl_dag!
@available(OSX 10.8, *)
func clReleaseDAGAPPLE(_ dag: cl_dag!)
@available(OSX 10.8, *)
@discardableResult
func clGetDAGNodeAPPLE(_ d: cl_dag!, _ f: cl_kernel!, _ args: UnsafeMutablePointer<cl_dag_node>!, _ arg_indices: UnsafeMutablePointer<UInt32>!, _ nargs: UInt32) -> cl_dag_node
@available(OSX 10.8, *)
@discardableResult
func clCreateKernelFromDAGAPPLE(_ d: cl_dag!, _ n: cl_uint, _ list: UnsafePointer<cl_device_id?>!) -> cl_kernel!
var cl_APPLE_command_queue_priority: Int32 { get }
var cl_APPLE_command_queue_select_compute_units: Int32 { get }
typealias cl_queue_properties_APPLE = Int
@available(OSX 10.8, *)
@discardableResult
func clCreateCommandQueueWithPropertiesAPPLE(_ _: cl_context!, _ _: cl_device_id!, _ _: UnsafePointer<cl_queue_properties_APPLE>!, _ _: UnsafeMutablePointer<cl_int>!) -> cl_command_queue!
var CL_QUEUE_PRIORITY_APPLE: Int32 { get }
var CL_QUEUE_NUM_COMPUTE_UNITS_APPLE: Int32 { get }
var CL_QUEUE_PRIORITY_BACKGROUND_APPLE: Int32 { get }
var CL_QUEUE_PRIORITY_DEFAULT_APPLE: Int32 { get }
var CL_OBJECT_NAME_APPLE: Int32 { get }
