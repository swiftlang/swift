//===--- TensorFlow.cpp - Runtime support for TensorFlow --------*- c++ -*-===//
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

#include "swift/Runtime/TensorFlow.h"
#include "tensorflow/c/c_api.h"
#include "tensorflow/c/c_api_experimental.h"
#include "tensorflow/c/eager/c_api.h"

using namespace swift;

SWIFT_RUNTIME_EXPORT
void *swift_tfc_TF_NewStatus() { return TF_NewStatus(); }

SWIFT_RUNTIME_EXPORT
void swift_tfc_TF_DeleteStatus(void *status) {
  TF_DeleteStatus(reinterpret_cast<TF_Status *>(status));
}

SWIFT_RUNTIME_EXPORT
void *swift_tfc_TFE_NewOp(void *ctx, const char *op_or_function_name,
                          void *status) {
  return TFE_NewOp(reinterpret_cast<TFE_Context *>(ctx), op_or_function_name,
                   reinterpret_cast<TF_Status *>(status));
}

SWIFT_RUNTIME_EXPORT
void swift_tfc_TFE_DeleteOp(void *op) {
  TFE_DeleteOp(reinterpret_cast<TFE_Op *>(op));
}

SWIFT_RUNTIME_EXPORT
void swift_tfc_TFE_OpSetAttrType(void *op, const char *attr_name,
                                 int32_t value) {
  return TFE_OpSetAttrType(reinterpret_cast<TFE_Op *>(op), attr_name,
                           static_cast<TF_DataType>(value));
}

SWIFT_RUNTIME_EXPORT
void swift_tfc_TFE_OpSetAttrTensor(void *op, const char *attr_name,
                                   void *tensor, void *status) {
  TFE_OpSetAttrTensor(reinterpret_cast<TFE_Op *>(op), attr_name,
                      reinterpret_cast<TF_Tensor *>(tensor),
                      reinterpret_cast<TF_Status *>(status));
}

SWIFT_RUNTIME_EXPORT
void swift_tfc_TF_DeleteTensor(void *tensor) {
  TF_DeleteTensor(reinterpret_cast<TF_Tensor *>(tensor));
}

SWIFT_RUNTIME_EXPORT
void *swift_tfc_CreateScalarFloatTensor(int32_t val) {
  auto *tensor =
      TF_AllocateTensor(TF_FLOAT, /*shape.data()*/ nullptr, /*shape.size()*/ 0,
                        TF_DataTypeSize(TF_FLOAT) * 1);
  auto *ptr = reinterpret_cast<char *>(TF_TensorData(tensor));
  *reinterpret_cast<float *>(ptr) = static_cast<float>(val);
  return tensor;
}

SWIFT_RUNTIME_EXPORT
void swift_tfc_TFE_Execute(void *op, void **retvals, int32_t *num_retvals,
                           void *status) {
  int int_num_retvals = *num_retvals;
  TFE_Execute(reinterpret_cast<TFE_Op *>(op),
              reinterpret_cast<TFE_TensorHandle **>(retvals), &int_num_retvals,
              reinterpret_cast<TF_Status *>(status));
  *num_retvals = int_num_retvals;
}
