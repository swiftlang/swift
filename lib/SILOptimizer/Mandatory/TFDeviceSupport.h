//===--- TFDeviceSupport.h - TensorFlow device management -------*- C++ -*-===//
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
// This defines the abstractions for representing TF device types, device
// placement for graph_op insts, and device partitioning API.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_TFDEVICESUPPORT_H
#define SWIFT_SILOPTIMIZER_TFDEVICESUPPORT_H

#include "swift/SIL/GraphFunctionDeviceInfo.h"
#include "swift/SIL/GraphOperationInfo.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {
namespace tf {
class GraphOperationBuilder;

class DevicePartitionerImpl;
struct GraphOperationInfo;

StringRef getDeviceString(const GraphOperationInfo &graphOpInfo);

DeviceType getDeviceType(const GraphOperationInfo &graphOpInfo);

/// The returned string can be used to construct SIL function names.
static inline std::string getDeviceShortName(DeviceType deviceType) {
  switch (deviceType) {
  case DeviceType::CPU:
    return "CPU";
  case DeviceType::GPU:
    return "GPU";
  case DeviceType::TPU:
    return "TPU";
  case DeviceType::ALL:
    return "ALL";
  case DeviceType::INVALID:
    llvm_unreachable("Unsupported device type");
  }
}

/// Partitions an accelerator SIL function into a set of per-device SIL
/// functions.
class DevicePartitioner {
  DevicePartitionerImpl *impl;

public:
  DevicePartitioner(SILTransform &transform, SILFunction &srcFn,
                    const GraphFunctionDeviceInfo &deviceInfo,
                    int &nextTensorTransferId);

  ~DevicePartitioner();

  /// Returns a function extracted from `srcFn`, specialized on `deviceType`.
  ///
  /// For example, say `fn` returns a+b, where a and b and constant tensors,
  /// and a is placed on GPU.
  /// - The extracted function for GPU device has the constant node a, fed
  /// into
  ///   a _Send() node to CPU.
  /// - The extracted function for CPU device has _Recv node from GPU to read
  ///   a, and adds its output with const tensor b to produce the sum result.
  SILFunction *extractFunctionForDevice(DeviceType deviceType);
};

} // end namespace tf
} // end namespace swift

#endif
