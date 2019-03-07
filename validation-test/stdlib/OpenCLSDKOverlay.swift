// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx

// Translated from standard OpenCL hello.c program

// Abstract:   A simple "Hello World" compute example showing basic usage of OpenCL which
//             calculates the mathematical square (X[i] = pow(X[i],2)) for a buffer of
//             floating point values.
//             
//
// Version:    <1.0>
//
// Disclaimer: IMPORTANT:  This Apple software is supplied to you by Apple Inc. ("Apple")
//             in consideration of your agreement to the following terms, and your use,
//             installation, modification or redistribution of this Apple software
//             constitutes acceptance of these terms.  If you do not agree with these
//             terms, please do not use, install, modify or redistribute this Apple
//             software.
//
//             In consideration of your agreement to abide by the following terms, and
//             subject to these terms, Apple grants you a personal, non - exclusive
//             license, under Apple's copyrights in this original Apple software (the
//             "Apple Software"), to use, reproduce, modify and redistribute the Apple
//             Software, with or without modifications, in source and / or binary forms
//             provided that if you redistribute the Apple Software in its entirety and
//             without modifications, you must retain this notice and the following text
//             and disclaimers in all such redistributions of the Apple Software. Neither
//             the name, trademarks, service marks or logos of Apple Inc. may be used to
//             endorse or promote products derived from the Apple Software without specific
//             prior written permission from Apple.  Except as expressly stated in this
//             notice, no other rights or licenses, express or implied, are granted by
//             Apple herein, including but not limited to any patent rights that may be
//             infringed by your derivative works or by other works in which the Apple
//             Software may be incorporated.
//
//             The Apple Software is provided by Apple on an "AS IS" basis.  APPLE MAKES NO
//             WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED
//             WARRANTIES OF NON - INFRINGEMENT, MERCHANTABILITY AND FITNESS FOR A
//             PARTICULAR PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS USE AND OPERATION
//             ALONE OR IN COMBINATION WITH YOUR PRODUCTS.
//
//             IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL OR
//             CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
//             SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//             INTERRUPTION) ARISING IN ANY WAY OUT OF THE USE, REPRODUCTION, MODIFICATION
//             AND / OR DISTRIBUTION OF THE APPLE SOFTWARE, HOWEVER CAUSED AND WHETHER
//             UNDER THEORY OF CONTRACT, TORT (INCLUDING NEGLIGENCE), STRICT LIABILITY OR
//             OTHERWISE, EVEN IF APPLE HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// Copyright (C) 2008 Apple Inc. All Rights Reserved.
//


import OpenCL
import StdlibUnittest


import Foundation
import Darwin

let KernelSource = "\n" +
"__kernel void square(                                                       \n" +
"   __global float* input,                                              \n" +
"   __global float* output,                                             \n" +
"   const unsigned int count)                                           \n" +
"{                                                                      \n" +
"   int i = get_global_id(0);                                           \n" +
"   if (i < count)                                                      \n" +
"       output[i] = input[i] * input[i];                                \n" +
"}                                                                      \n" +
"\n"


let DATA_SIZE = 1024

var tests = TestSuite("MiscSDKOverlay")

tests.test("clSetKernelArgsListAPPLE") {
  var err: cl_int                            // error code returned from api calls
  
  var data = [Float](repeating: 0, count: DATA_SIZE)    // original data set given to device
  var results = [Float](repeating: 0, count: DATA_SIZE) // results returned from device
  var correct: Int               // number of correct results returned

  var global: size_t                      // global domain size for our calculation
  var local: size_t = 0                       // local domain size for our calculation

  var device_id: cl_device_id? = nil             // compute device id
  var context: cl_context?                 // compute context
  var commands: cl_command_queue?          // compute command queue
  var program: cl_program?                 // compute program
  var kernel: cl_kernel?                   // compute kernel
  
  // Fill our data set with random float values
  //
  var count = DATA_SIZE
  for i in 0..<count {
    data[i] = 0.0
  }
  
  // Connect to a compute device
  //
  err = clGetDeviceIDs(nil, cl_device_type(CL_DEVICE_TYPE_ALL), 1, &device_id, nil)
  if (err != CL_SUCCESS)
  {
    print("Error: Failed to create a device group!")
    exit(EXIT_FAILURE)
  }
  
  // Create a compute context 
  //
  context = clCreateContext(nil, 1, &device_id, nil, nil, &err)
  if (context == nil)
  {
    print("Error: Failed to create a compute context!")
    exit(EXIT_FAILURE)
  }

  // Create a command commands
  //
  commands = clCreateCommandQueue(context, device_id, 0, &err)
  if (commands == nil)
  {
    print("Error: Failed to create a command commands!")
    exit(EXIT_FAILURE)
  }

  // Create the compute program from the source buffer
  //
  program = KernelSource.withCString {
    (p: UnsafePointer<CChar>) -> cl_program in
    var s: UnsafePointer? = p
    return withUnsafeMutablePointer(to: &s) {
      return clCreateProgramWithSource(context, 1, $0, nil, &err)
    }
  }
  if (program == nil)
  {
    print("Error: Failed to create compute program!")
    exit(EXIT_FAILURE)
  }

  // Build the program executable
  //
  err = clBuildProgram(program, 0, nil, nil, nil, nil)
  if (err != CL_SUCCESS)
  {
    var len: Int = 0
    
    var buffer = [CChar](repeating: 0, count: 2048)

    print("Error: Failed to build program executable!")
    clGetProgramBuildInfo(
      program, device_id, cl_program_build_info(CL_PROGRAM_BUILD_LOG), 2048, &buffer, &len)
    print("\(String(cString: buffer))")
    exit(1)
  }

  // Create the compute kernel in the program we wish to run
  //
  kernel = clCreateKernel(program, "square", &err)
  if (kernel == nil || err != cl_int(CL_SUCCESS))
  {
    print("Error: Failed to create compute kernel!")
    exit(1)
  }

  // Create the input and output arrays in device memory for our calculation
  //
  guard var input = clCreateBuffer(context,  cl_mem_flags(CL_MEM_READ_ONLY),  MemoryLayout<Float>.size * count, nil, nil),
        var output = clCreateBuffer(context, cl_mem_flags(CL_MEM_WRITE_ONLY), MemoryLayout<Float>.size * count, nil, nil) else {
    print("Error: Failed to allocate device memory!")
    exit(1)
  }
  
  // Write our data set into the input array in device memory 
  //
  err = clEnqueueWriteBuffer(commands, input, cl_bool(CL_TRUE), 0, MemoryLayout<Float>.size * count, data, 0, nil, nil)
  if (err != CL_SUCCESS)
  {
    print("Error: Failed to write to source array!")
    exit(1)
  }

  // Set the arguments to our compute kernel
  //
  err = 0
  err = withUnsafePointer(to: &input) {
    inputPtr in withUnsafePointer(to: &output) {
      outputPtr in withUnsafePointer(to: &count) {
        countPtr in
        clSetKernelArgsListAPPLE(
          kernel!, 3,
          0, MemoryLayout<cl_mem>.size, inputPtr,
          1, MemoryLayout<cl_mem>.size, outputPtr,
          2, MemoryLayout.size(ofValue: countPtr.pointee), countPtr)
      }
    }
  }

  
  if (err != CL_SUCCESS)
  {
    print("Error: Failed to set kernel arguments! \(err)")
    exit(1)
  }

  // Get the maximum work group size for executing the kernel on the device
  //
  err = clGetKernelWorkGroupInfo(kernel, device_id, cl_kernel_work_group_info(CL_KERNEL_WORK_GROUP_SIZE), MemoryLayout.size(ofValue: local), &local, nil)
  if (err != CL_SUCCESS)
  {
    print("Error: Failed to retrieve kernel work group info! \(err)")
    exit(1)
  }

  // Execute the kernel over the entire range of our 1d input data set
  // using the maximum number of work group items for this device
  //
  global = count
  err = clEnqueueNDRangeKernel(commands, kernel, 1, nil, &global, &local, 0, nil, nil)
  if (err != 0)
  {
    print("Error: Failed to execute kernel!")
    exit(EXIT_FAILURE)
  }

  // Wait for the command commands to get serviced before reading back results
  //
  clFinish(commands)

  // Read back the results from the device to verify the output
  //
  err = clEnqueueReadBuffer(commands, output, cl_bool(CL_TRUE), 0, MemoryLayout<Float>.size * count, &results, cl_uint(0), nil, nil)
  if (err != CL_SUCCESS)
  {
    print("Error: Failed to read output array! \(err)")
    exit(1)
  }
  
  // Validate our results
  //
  correct = 0
  for i in 0..<count
  {
    if (results[i] == data[i] * data[i]) {
      correct += 1
    }
  }
  
  // Print a brief summary detailing the results
  //
  print("Computed '\(correct)/\(count)' correct values!")
  
  // Shutdown and cleanup
  //
  clReleaseMemObject(input)
  clReleaseMemObject(output)
  clReleaseProgram(program)
  clReleaseKernel(kernel)
  clReleaseCommandQueue(commands)
  clReleaseContext(context)
}

runAllTests()
