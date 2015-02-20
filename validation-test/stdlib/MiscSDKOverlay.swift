//===--- MiscSDKOverlay.swift - Tests for small SDK Overlays --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Lots of overlays are too small to warrant giving their own test
//  file; test them here instead.
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift

import OpenCL
import StdlibUnittest
import Foundation

var tests = TestSuite("MiscSDKOverlay")

tests.test("clSetKernelArgsListAPPLE") {
  // this test doesn't check functionality, but it does make sure we
  // can call the function.
  let kernel = "\n".join(
    [ "// Simple OpenCL kernel that squares an input array.", 
      "// This code is stored in a file called mykernel.cl.", 
      "// You can name your kernel file as you would name any other", 
      "// file.  Use .cl as the file extension for all kernel", 
      "// source files.", 
      " ", 
      "// Kernel block.                                      //   1", 
      "kernel void square(                                   //   2", 
      "                   global float* input,               //   3", 
      "                   global float* output)", 
      "{", 
      "    size_t i = get_global_id(0);", 
      "    output[i] = input[i] * input[i];", 
      "}"
    ])
  let input: [Float] = [1, 2, 3, 4]
  var output: [Float] = input

  // FIXME: Avoiding withUnsafeMutableBufferPointer as a workaround
  // for <rdar://19898730> typecheck #FAIL
  func testKernelOnPointers(
    in_: UnsafePointer<Float>, out: UnsafeMutablePointer<Float>
  ) -> cl_int {
    return kernel.withCString {
      clSetKernelArgsListAPPLE(
        cl_kernel($0), 2,
        0, sizeofValue(in_), in_,
        1, sizeofValue(out), out)
    }
  }

  testKernelOnPointers(input, &output)
}
