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
