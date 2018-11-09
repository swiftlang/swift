//===-- StringOps.swift --------------------------------------*- swift -*-===//
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
// This file contains definitions of most string tensor operations.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Element-wise binary comparison
//===----------------------------------------------------------------------===//

public extension StringTensor {
  /// Computes `self == other` element-wise.
  /// - Note: `elementsEqual` supports broadcasting.
  @inlinable @inline(__always)
  func elementsEqual(_ other: StringTensor) -> Tensor<Bool> {
    return #tfop("Equal", self.handle, other.handle,
                 T$dtype: String.tensorFlowDataType)
  }
}
