//===-- TensorCore.swift --------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the TensorCore type and the primitive Tensor "op"
// functions, in a simple and predictable style that can be mapped onto
// TensorFlow ops.
//
//===----------------------------------------------------------------------===//

import Swift

/// This type is the internal implementation goop that should only be used by
/// Tensor.  It is the primitive thing that reflects the state managed by the
/// tf-compiler partitioner.
struct TensorCore<Element : TensorElementProtocol> {
  // FIXME: Implement
  private var tmpState : UnsafePointer<Int>? = nil
}
