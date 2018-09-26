//===--- TensorFlow.h - Runtime support for TensorFlow ----------*- c++ -*-===//
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
// SWIFT_ENABLE_TENSORFLOW
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"
#include <stdint.h>

namespace swift {

SWIFT_RUNTIME_EXPORT
void *swift_tfc_TF_NewStatus();

SWIFT_RUNTIME_EXPORT
void swift_tfc_TF_DeleteStatus(void *status);

SWIFT_RUNTIME_EXPORT
void *swift_tfc_TFE_NewOp(void *ctx, const char *op_or_function_name,
                          void *status);

SWIFT_RUNTIME_EXPORT
void swift_tfc_TFE_DeleteOp(void *op);

SWIFT_RUNTIME_EXPORT
void swift_tfc_TFE_OpSetAttrType(void *op, const char *attr_name,
                                 int32_t value);

SWIFT_RUNTIME_EXPORT
void swift_tfc_TFE_OpSetAttrTensor(void *op, const char *attr_name,
                                   void *tensor, void *status);

SWIFT_RUNTIME_EXPORT
void swift_tfc_TF_DeleteTensor(void *tensor);

// Caller owns the returned tensor.
// TODO: Generalize to create tensors from other shapes and dtypes.
SWIFT_RUNTIME_EXPORT void *swift_tfc_CreateScalarFloatTensor(int32_t val);

SWIFT_RUNTIME_EXPORT
void swift_tfc_TFE_Execute(void *op, void **retvals, int32_t *num_retvals,
                           void *status);

} // end namespace swift
