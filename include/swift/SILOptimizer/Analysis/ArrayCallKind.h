//===--- ArrayCallKind.h -------------------------------------- -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef ARRAY_CALL_KIND_H
#define ARRAY_CALL_KIND_H

/// The kind of array operation identified by looking at the semantics attribute
/// of the called function.
enum class ArrayCallKind {
  kNone = 0,
  kArrayPropsIsNativeTypeChecked,
  kCheckSubscript,
  kCheckIndex,
  kGetCount,
  kGetCapacity,
  kGetElement,
  kGetElementAddress,
  kMakeMutable,
  kEndMutation,
  kMutateUnknown,
  kReserveCapacityForAppend,
  kWithUnsafeMutableBufferPointer,
  kAppendContentsOf,
  kAppendElement,
  // The following two semantic function kinds return the result @owned
  // instead of operating on self passed as parameter. If you are adding
  // a function, and it has a self parameter, make sure that it is defined
  // before this comment.
  kArrayInit,
  kArrayInitEmpty,
  kArrayUninitialized,
  kArrayUninitializedIntrinsic,
  kArrayFinalizeIntrinsic
};

#endif
