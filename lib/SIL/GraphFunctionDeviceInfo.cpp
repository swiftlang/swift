//===- GraphFunctionDeviceInfo.cpp - Utils for setting op devices -* C++ *-===//
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
// This file defines utilities for assigning ops to devices.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/GraphFunctionDeviceInfo.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SIL/GraphOperationBuilder.h"
#include "swift/SIL/GraphOperationInfo.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "tensorflow/c/c_api_experimental.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace tf;

// Only DataType attributes are considered in this CanRunOnDevice property. All
// others are currently not relevant for kernel selection.
bool CanRunOnDevice(llvm::StringRef opType,
                    llvm::ArrayRef<GraphOperationAttribute> attributes,
                    const char *deviceType) {
  auto *builder = TF_NewAttrBuilder(std::string(opType).c_str());

  unsigned dtypeAttr = 0;
  for (const auto &attr : attributes) {
    auto attrInfo = GraphOperationInfo::decodeArgumentName(attr.name.str());
    std::string name = attrInfo.first.str();
    auto attrValue = attr.value;
    if (attrInfo.second ==
        GraphOperationInfo::ArgumentLowering::TFDataTypeAttribute) {
      switch (attrValue.getKind()) {
      case SymbolicValue::Integer:
        dtypeAttr = getTFDataType(attrValue);
        TF_AttrBuilderSetType(builder, name.c_str(), (TF_DataType)dtypeAttr);
        break;
      case SymbolicValue::Array: {
        CanType eltTy;
        SmallVector<TF_DataType, 4> types;
        for (auto elt : attrValue.getArrayValue(eltTy))
          types.push_back((TF_DataType)getTFDataType(elt));
        TF_AttrBuilderSetTypeList(builder, name.c_str(), types.data(),
                                  types.size());
        break;
      }
      default:
        llvm_unreachable(
            "only integers and arrays are possible for TF_DataType attrs");
      }
    }
  }

  auto *status = TF_NewStatus();
  TF_AttrBuilderCheckCanRunOnDevice(builder, deviceType, status);
  bool isOk = TF_GetCode(status) == TF_OK;
  TF_DeleteAttrBuilder(builder);
  TF_DeleteStatus(status);
  return isOk;
}

static llvm::cl::opt<bool> TFTargetTPU(
    "tf-target-tpu", llvm::cl::init(false),
    llvm::cl::desc("If true, target TPU in the generated TF graph. This flag "
                   "is used for unit testing only"));
static llvm::cl::opt<bool> TFTargetGPU(
    "tf-target-gpu", llvm::cl::init(false),
    llvm::cl::desc("If true, target GPU in the generated TF graph. This flag "
                   "is used for unit testing only"));

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &Context, SourceLoc loc,
                                   Diag<T...> diag, U &&... args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

/// Scan the specified function, looking for logic that configures the current
/// graph.
GraphFunctionDeviceInfo
GraphFunctionDeviceInfo::getForFunction(SILFunction &fn,
                                        bool removeConfigInst) {
  DeviceType deviceType = DeviceType::CPU;
  bool isTPUInfeedEnabled = false;

  SILInstruction *firstFound = nullptr;
  SmallVector<SILInstruction *, 4> configureInsts;
  for (auto &bb : fn) {
    for (auto &inst : bb) {
      // Scan for the device configuration ops if present.
      auto graphOpInst = dyn_cast<GraphOperationInst>(&inst);
      if (!graphOpInst)
        continue;
      GraphOperationInfo opInfo(graphOpInst);
      if (!isConfigOp(opInfo))
        continue;

      configureInsts.push_back(&inst);

      // If we found one, make sure we don't have more than one.
      if (firstFound) {
        diagnose(fn.getASTContext(), inst.getLoc().getSourceLoc(),
                 diag::tf_multiple_device);
        diagnose(fn.getASTContext(), firstFound->getLoc().getSourceLoc(),
                 diag::tf_multiple_device_prev);
        continue;
      }

      // Otherwise, remember this one and decode it.
      firstFound = &inst;

      // Eventually we'll support multiple different configuration ops, so
      // we recheck the opcode here.
      if (opInfo.getOperationName() == "tfc.configureTPU") {
        // Decode: tfc.configureTPU(isInfeedEnabled: bool)
        deviceType = DeviceType::TPU;
        auto infeedEnabled = cast<IntegerLiteralInst>(inst.getOperand(0));
        isTPUInfeedEnabled = !infeedEnabled->getValue().isNullValue();
      } else if (opInfo.getOperationName() == "tfc.configureGPU") {
        deviceType = DeviceType::GPU;
      } else {
        assert(opInfo.getOperationName() == "tfc.configureCPU" &&
               "unknown device configuration op");
        deviceType = DeviceType::CPU;
      }
    }
  }

  // If the program didn't specify, fall back to the command line option.
  if (!firstFound) {
    // At most one of these test-only flags should be set.
    // FIXME: Change this to a mutually exclusive flag setting an enum.
    assert(!TFTargetTPU || !TFTargetGPU);
    if (TFTargetTPU)
      deviceType = DeviceType::TPU;
    if (TFTargetGPU)
      deviceType = DeviceType::GPU;
  }

  // These instructions are not relevant to later compiler passes in TFPartition
  // and TFLowerGraph. Removing them so that the later passes need not deal with
  // this special builtin type.
  if (removeConfigInst) {
    for (auto *configureInst : configureInsts) {
      assert(!configureInst->hasUsesOfAnyResult());
      recursivelyDeleteTriviallyDeadInstructions(configureInst, /*Force*/ true);
    }
  }
  return GraphFunctionDeviceInfo(deviceType, isTPUInfeedEnabled);
}

/// Whether this is an op that configures the function's device.
bool GraphFunctionDeviceInfo::isConfigOp(const GraphOperationInfo &opInfo) {
  return opInfo.getOperationName() == "tfc.configureTPU" ||
         opInfo.getOperationName() == "tfc.configureGPU" ||
         opInfo.getOperationName() == "tfc.configureCPU";
}

std::string GraphFunctionDeviceInfo::handleDevicePlacement(
    StringRef opType, StringRef opDevice,
    llvm::ArrayRef<GraphOperationAttribute> attributes) {
  DeviceType chosenDevice;
  if (!opDevice.empty())
    chosenDevice = getOpDeviceType(opDevice);
  else
    chosenDevice = chooseDevice(opType, attributes);

  markDeviceUsed(chosenDevice);
  return getDeviceString(chosenDevice);
}

void GraphFunctionDeviceInfo::handleDevicePlacement(
    StringRef opType, StringRef opDevice, ASTContext &ctx,
    GraphOperationBuilder *opBuilder) {

  auto deviceString =
      handleDevicePlacement(opType, opDevice, opBuilder->getAttributes());

  // Example output SIL:
  // graph_op "Const"() {dtype: $Float, value$tensor: f32 0x3F800000 /* 1 */,
  //   __device: "/device:CPU:0"}
  // TODO: Use integer device ID's instead of strings?
  opBuilder->addAttribute(
      {ctx.getIdentifier(TF_DEVICE_ATTR),
       SymbolicValue::getString(deviceString, ctx.getAllocator())});
}

DeviceType GraphFunctionDeviceInfo::chooseDevice(
    llvm::StringRef opType,
    llvm::ArrayRef<GraphOperationAttribute> attributes) const {
  if (opType == "tfc.RecvFromHost" || opType == "tfc.SendToHost")
    return DeviceType::CPU;

  // TODO: A similar statement might be necessary for TPU.
  if (primaryDeviceType == DeviceType::GPU) {
    if (CanRunOnDevice(opType, attributes, "GPU")) {
      return DeviceType::GPU;
    }
    return DeviceType::CPU;
  }
  return primaryDeviceType;
}
