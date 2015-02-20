//===----------------------------------------------------------------------===//
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

@exported import OpenCL // Clang module

public let openCLOverlayWorks = true

@availability(OSX, introduced=10.7)
public func clSetKernelArgsListAPPLE(
  kernel: cl_kernel, uint: cl_uint, args: CVarArgType...
) -> cl_int {
  // The variable arguments are num_args arguments that are the following:
  //      cl_uint arg_indx,
  //      size_t arg_size,
  //      const void *arg_value,
  return withVaList(args) {  clSetKernelArgsVaListAPPLE(kernel, uint, $0) }
}
