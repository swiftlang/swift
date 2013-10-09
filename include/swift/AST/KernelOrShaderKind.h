//===--- KernelOrShaderKind.h - Kernel/Shader Kind Enum ---------*- C++ -*-===//
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
// This file defines the KernelOrShaderKind enumeration.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_KERNELORSHADERKIND_H
#define SWIFT_KERNELORSHADERKIND_H

namespace swift {

/// Marks if a function is a compute kernel, vertex shader, or fragment shader.
enum class KernelOrShaderKind : unsigned char {
  /// Not a kernel or shader.
  Default = 0,
  /// A compute kernel.
  Kernel,
  /// A vertex shader.
  Vertex,
  /// A fragment shader.
  Fragment
};

} // end namespace swift

#endif // LLVM_SWIFT_KERNELORSHADERKIND_H
