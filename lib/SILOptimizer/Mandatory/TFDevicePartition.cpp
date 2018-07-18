//===--- TFDevicePartition.cpp - Partition TF graph on devices ----------===//
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
// This pass partitions an accelerator SIL function into a set of per-device SIL
// functions.
//
//===----------------------------------------------------------------------===//

#include "TFDeviceSupport.h"
#include "TFUtilities.h"
#ifdef SWIFT_ENABLE_TENSORFLOW
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace tf;

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

//===----------------------------------------------------------------------===//
// Device Partitioning Utilities
//===----------------------------------------------------------------------===//

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
      auto tfopInfo = SILTensorOpInfo::decode(&inst);
      if (!tfopInfo)
        continue;
      bool isConfigOp = tfopInfo->opName == "tfc.configureTPU" ||
                        tfopInfo->opName == "tfc.configureGPU";
      if (!isConfigOp)
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
      if (tfopInfo->opName == "tfc.configureTPU") {
        // Decode: tfc.configureTPU(isInfeedEnabled: bool)
        deviceType = DeviceType::TPU;
        auto infeedEnabled =
            cast<IntegerLiteralInst>(tfopInfo->getAttrOperand(0));
        isTPUInfeedEnabled = !infeedEnabled->getValue().isNullValue();
      } else {
        assert(tfopInfo->opName == "tfc.configureGPU" &&
               "unknown device configuration op");
        deviceType = DeviceType::GPU;
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
  } else if (auto *outs = getTFDumpIntermediateStream()) {
    *outs << "Targeting device " << getDeviceString(deviceType)
          << " for accelerator program, based on config: \n";
    firstFound->print(*outs);
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

void GraphFunctionDeviceInfo::handleDevicePlacement(
    StringRef opType, StringRef opDevice, ASTContext &ctx,
    SmallVectorImpl<GraphOperationAttribute> &attributes) {
  DeviceType chosenDevice;
  if (!opDevice.empty())
    chosenDevice = getOpDeviceType(opDevice);
  else
    chosenDevice = chooseDevice(opType);

  markDeviceUsed(chosenDevice);

  // Example output SIL:
  // graph_op "Const"() {dtype: $Float, value$tensor: f32 0x3F800000 /* 1 */,
  //   __device: "/device:CPU:0"}
  auto deviceString = getDeviceString(chosenDevice);
  // TODO: Use integer device ID's instead of strings?
  attributes.push_back(
      {ctx.getIdentifier(DEVICE_ATTR),
       SymbolicValue::getString(deviceString, ctx.getAllocator())});
}

//===----------------------------------------------------------------------===//
// Device Partitioner and Cloner ClassesPartitioning Utilities
//===----------------------------------------------------------------------===//

namespace {

/// Extracts from an input accelerator SIL function a device-specific SIL
/// function.
/// This class is adapted from PartitionCloner.
class DevicePartitionCloner
    : public SILClonerWithScopes<DevicePartitionCloner> {
  SILFunction& srcFn;

  const GraphFunctionDeviceInfo &deviceInfo;

  /// The device for us to extract the SIL computation in.
  const DeviceType thisDeviceType;

  /// The set of ops in `srcFn` to run on `thisDeviceType`.
  const SmallPtrSetImpl<SILInstruction *> &targetOps;

 public:
   DevicePartitionCloner(SILFunction &srcFn,
                         const GraphFunctionDeviceInfo &deviceInfo,
                         DeviceType thisDeviceType,
                         const SmallPtrSetImpl<SILInstruction *> &targetOps,
                         SILFunction &NewFn)
       : SILClonerWithScopes(NewFn), srcFn(srcFn), deviceInfo(deviceInfo),
         thisDeviceType(thisDeviceType), targetOps(targetOps) {
     assert(thisDeviceType != DeviceType::ALL);
   }

  /// Extracts the device-specific computation into `NewFn` above.
  void cloneFunction();

  void visitGraphOperationInst(GraphOperationInst *inst);
  void visitTFOpInst(SILTensorOpInfo &tfopInfo);

  void visitBuiltinInst(BuiltinInst *inst) {
    if (auto tfopInfo = SILTensorOpInfo::decode(inst)) {
      if (targetOps.count(inst))
        visitTFOpInst(tfopInfo.getValue());
      return;
    }

    // Other kinds of builtins such as "tf_tensor_to_i1" are cloned over
    // directly.
    SILClonerWithScopes::visitBuiltinInst(inst);
  }

  // These get special handling, they are only used as operands to tfops.
  void visitIntegerLiteralInst(IntegerLiteralInst *inst) {}
  void visitFloatLiteralInst(FloatLiteralInst *inst) {}
  void visitMetatypeInst(MetatypeInst *inst) {}
  void visitStringLiteralInst(StringLiteralInst *inst) {}
  void visitFunctionRefInst(FunctionRefInst *inst) {}

  void visitTupleInst(TupleInst *inst) {
    // Tuples never exist in the graph except when they are the argument to
    // the return instruction.
    assert(inst->hasOneUse() &&
           isa<ReturnInst>(inst->getSingleUse()->getUser()) &&
           "Unexpected tuple_inst in DevicePartitionCloner");
    if (isPrimaryFn()) {
      // For a non-primary function, we'll create an empty tuple when cloning
      // the return inst.
      SILClonerWithScopes::visitTupleInst(inst);
    }
  }

  // These instructions are cloned over
  void visitUncheckedRefCastInst(UncheckedRefCastInst *inst) {
    if (!targetOps.count(inst)) return;
    SILClonerWithScopes::visitUncheckedRefCastInst(inst);
  }

  void visitTupleExtractInst(TupleExtractInst *inst) {
    if (!targetOps.count(inst)) return;
    SILClonerWithScopes::visitTupleExtractInst(inst);
  }

  void visitStructExtractInst(StructExtractInst *inst) {
    if (!targetOps.count(inst)) return;
    SILClonerWithScopes::visitStructExtractInst(inst);
  }

  void visitReturnInst(ReturnInst *inst) {
    if (!isPrimaryFn()) {
      auto &B = getBuilder();
      auto loc = remapLocation(getUserSourceLocation(inst->getDebugLocation()));
      auto emptyTuple = B.createTuple(loc, {});
      B.createReturn(loc, emptyTuple);
      return;
    }

    SILClonerWithScopes::visitReturnInst(inst);
  }

 private:
  bool isPrimaryFn() const {
    return deviceInfo.primaryDeviceType == thisDeviceType;
  }

  /// Check to see if the argument was marked in a way that indicates we should
  /// copy it over to the tensor program.
  bool shouldCloneArgument(SILArgument *bbArg) const {
    // For now we always clone BB args, so that control flow can be handled
    // properly in all cases.
    //
    // TODO: Optimize the partitioned functions by pruning away some of the
    // cloned bbArgs when applicable.
    return true;
  }

  /// A TensorTransfer builtin sends a tensor value from a specific src TF
  /// device to one or ALL dest TF devices, where the src and dest devices must
  /// be different.
  /// For example, if tensor value x is produced on CPU, and has a use on GPU of
  /// form foo(x), we will insert a TensorTransfer builtin so that the code
  /// becomes:
  ///   x = <inst defining x, running on CPU>
  ///   x' = TensorTransfer(x, ...)  // src is CPU, and dest is GPU
  ///   foo(x') // this runs on GPU
  ///
  /// This builtin helps maintain the invariant that for any instruction I
  /// running on some device D, for any operand OP of I, OP must be present on D
  /// (either because OP is produced on D, or it is transferred via this
  /// builtin).
  ///
  /// `tensorShapeAttrIdx` points to the optional shape array attr in
  /// `graphOpInfo`. It is used when generating the TPU flavor of send/recv ops
  /// (i.e., infeed/outfeed).
  void visitTensorTransferInst(GraphOperationInfo &graphOpInfo);
  void addD2DSend(GraphOperationInfo &graphOpInfo, unsigned tensorShapeAttrIdx,
                  int transferId, DeviceType destDevice);
  void addD2DRecv(GraphOperationInfo &graphOpInfo, unsigned tensorShapeAttrIdx,
                  int transferId, DeviceType srcDevice);

  void initBlock(SILBasicBlock *BB);

  /// Move and clone code over from the input block into this block, inserting
  /// transfers between the host and destination code as necessary.
  void cloneBlock(SILBasicBlock *BB);

  SingleValueInstruction *cloneSingleInst(SingleValueInstruction *inst) {
    auto *ourInst = inst->clone();
    auto &B = getBuilder();
    auto loc = remapLocation(getUserSourceLocation(inst->getDebugLocation()));
    ourInst->setDebugLocation(B.getSILDebugLocation(loc));
    B.getInsertionBB()->push_back(ourInst);
    return ourInst;
  }

  SILValue cloneScalarOperand(SILValue operand) {
    assert(!isTensorFlowValue(operand->getType()));
    assert(isa<SingleValueInstruction>(operand));
    if (!ValueMap.count(operand)) {
      auto *newOperand = cloneSingleInst(cast<SingleValueInstruction>(operand));
      ValueMap[operand] = newOperand;
    }
    return this->remapValue(operand);
  }
};
}  // end anonymous namespace

void DevicePartitionCloner::visitGraphOperationInst(GraphOperationInst *inst) {
  // TODO: try and remove this special case.
  if (inst->getName().str() == "tf_tensor_to_i1") {
    SILClonerWithScopes::visitGraphOperationInst(inst);
    return;
  }

  GraphOperationInfo decoder(inst);
  SmallVector<GraphOperationInfo::InputMarker, 4> inputInfos;
  auto opName = decoder.decodeName(inputInfos);
  if (opName == "tfc.TensorTransfer") {
    assert(inputInfos.size() == 1);
    assert(inputInfos[0] == GraphOperationInfo::IM_Normal);
    visitTensorTransferInst(decoder);
    return;
  }

  auto deviceType = decoder.getDeviceType();

  // Skip this instruction if it isn't for the current device.
  if (deviceType != DeviceType::ALL && deviceType != thisDeviceType)
    return;

  auto &B = getBuilder();
  auto loc = remapLocation(getUserSourceLocation(inst->getDebugLocation()));

  // Clone it over.
  SmallVector<SILValue, 4> args;
  for (unsigned i = 0, e = inst->getNumOperands(); i != e; ++i) {
    auto opValue = inst->getOperand(i);
    assert(isTensorFlowValue(opValue->getType()) &&
           "ops should only use tensors");
    args.push_back(remapValue(opValue));
  }

  SmallVector<SILType, 2> resultTypes;
  for (auto r : inst->getResults())
    resultTypes.push_back(r->getType());

  auto newOp = B.createGraphOperation(loc, inst->getName(), args,
                                      inst->getAttributes(), resultTypes);

  for (unsigned i = 0, e = inst->getNumResults(); i != e; ++i)
    ValueMap[inst->getResult(i)] = newOp->getResult(i);
}

void DevicePartitionCloner::visitTFOpInst(SILTensorOpInfo &tfopInfo) {
  auto deviceString = tfopInfo.getDeviceString();
  auto deviceType = getOpDeviceType(deviceString);

  bool shouldRunInstOnThisDevice =
      deviceType == DeviceType::ALL || deviceType == thisDeviceType;
  if (!shouldRunInstOnThisDevice) {
    // Skip this inst
    return;
  }

  auto &B = getBuilder();
  auto *inst = tfopInfo.inst;
  auto loc = remapLocation(getUserSourceLocation(inst->getDebugLocation()));

  // Clone it over.
  SmallVector<SILValue, 4> args;
  assert(!isa<ApplyInst>(inst));
  for (unsigned i = 0, e = inst->getNumOperands(); i != e; ++i) {
    auto opValue = inst->getOperand(i);
    if (isTensorFlowValue(opValue->getType())) {
      args.push_back(remapValue(opValue));
    } else {
      args.push_back(cloneSingleInst(cast<SingleValueInstruction>(opValue)));
    }
  }

  auto newName = B.getASTContext().getIdentifier(tfopInfo.builtinName);
  auto result = B.createBuiltin(loc, newName, inst->getType(),
                                /*no substitutions*/ {}, args);
  ValueMap[inst] = result;
}

void DevicePartitionCloner::addD2DSend(GraphOperationInfo &graphOpInfo,
                                       unsigned tensorShapeAttrIdx,
                                       int transferId, DeviceType destDevice) {
  auto srcDevice = thisDeviceType;
  assert(srcDevice != DeviceType::ALL);
  assert(destDevice != DeviceType::ALL);
  assert(srcDevice != destDevice);
  auto &B = getBuilder();
  auto &ctx = B.getASTContext();
  auto *inst = graphOpInfo.inst;
  auto loc = remapLocation(getUserSourceLocation(inst->getDebugLocation()));

  // Insert a send inst, with type <T> (T) {int, str, str} -> ()
  std::string newInstName = "tfc.D2DTensorSend";
  newInstName +=
      GraphOperationInfo::getInputMarker(GraphOperationInfo::IM_Normal);

  auto &allocator = ctx.getAllocator();
  SmallVector<GraphOperationAttribute, 4> attributes;
  attributes.push_back({ctx.getIdentifier("transferId"),
                        SymbolicValue::getInteger(transferId, 32)});
  attributes.push_back(
      {ctx.getIdentifier("destDevice"),
       SymbolicValue::getString(getDeviceString(destDevice), allocator)});
  attributes.push_back(
      {ctx.getIdentifier(DEVICE_ATTR),
       SymbolicValue::getString(getDeviceString(thisDeviceType), allocator)});

  auto valueToSend = remapValue(inst->getOperand(0));
  if (inst->getNumAttributes() > tensorShapeAttrIdx)
    attributes.push_back(inst->getAttribute(tensorShapeAttrIdx));
  B.createGraphOperation(loc, ctx.getIdentifier(newInstName),
                         /*operands*/ {valueToSend}, attributes, {});
  // Do not update ValueMap since Send does not produce a value.
}

void DevicePartitionCloner::addD2DRecv(GraphOperationInfo &graphOpInfo,
                                       unsigned tensorShapeAttrIdx,
                                       int transferId, DeviceType srcDevice) {
  auto destDevice = thisDeviceType;
  assert(srcDevice != DeviceType::ALL);
  assert(destDevice != DeviceType::ALL);
  assert(srcDevice != destDevice);
  auto &B = getBuilder();
  auto &ctx = B.getASTContext();
  auto *inst = graphOpInfo.inst;
  auto loc = remapLocation(getUserSourceLocation(inst->getDebugLocation()));

  // Insert a recv inst, with type <T> {int, str, str} -> (T)
  std::string newInstName = "tfc.D2DTensorRecv";

  auto &allocator = ctx.getAllocator();
  SmallVector<GraphOperationAttribute, 4> attributes;
  attributes.push_back({ctx.getIdentifier("transferId"),
                        SymbolicValue::getInteger(transferId, 32)});
  attributes.push_back(
      {ctx.getIdentifier("srcDevice"),
       SymbolicValue::getString(getDeviceString(srcDevice), allocator)});
  attributes.push_back(
      {ctx.getIdentifier(DEVICE_ATTR),
       SymbolicValue::getString(getDeviceString(thisDeviceType), allocator)});

  auto valueTy = inst->getResults()[0]->getType();
  if (inst->getNumAttributes() > tensorShapeAttrIdx)
    attributes.push_back(inst->getAttribute(tensorShapeAttrIdx));

  auto valueToRecv = getSingleValueResult(inst);
  auto *transferInst =
      B.createGraphOperation(loc, ctx.getIdentifier(newInstName),
                             /*operands*/ {}, attributes, {valueTy});
  auto newValue = getSingleValueResult(transferInst);
  ValueMap[valueToRecv] = newValue;
}

void DevicePartitionCloner::visitTensorTransferInst(
    GraphOperationInfo &graphOpInfo) {
  auto *inst = graphOpInfo.inst;
  assert(inst->getNumResults() == 1);
  assert(inst->getNumOperands() == 1);
  assert(inst->getNumAttributes() == 3 || inst->getNumAttributes() == 4);

  int transferId = graphOpInfo.getIntAttr(0, "transferId");
  auto srcDeviceStr = graphOpInfo.getStringAttr(1, "srcDevice");
  auto srcDevice = getOpDeviceType(srcDeviceStr);
  auto destDeviceStr = graphOpInfo.getStringAttr(2, "destDevice");
  auto destDevice = getOpDeviceType(destDeviceStr);
  assert(srcDevice != destDevice);
  // This builtin cannot have src device set to ALL, but dest device can be ALL.
  assert(srcDevice != DeviceType::ALL);
  bool shouldRunTransferAsSrcDevice = srcDevice == thisDeviceType;
  bool shouldRunTransferAsDestDevice =
      destDevice == DeviceType::ALL || destDevice == thisDeviceType;
  if (!shouldRunTransferAsSrcDevice && !shouldRunTransferAsDestDevice)
    return;

  // The optional attr starts at attr 3.
  const unsigned tensorShapeAttrIdx = 3;
  if (!shouldRunTransferAsSrcDevice) {
    assert(shouldRunTransferAsDestDevice);
    addD2DRecv(graphOpInfo, tensorShapeAttrIdx, transferId, srcDevice);
    return;
  }

  // Run transfer as src device, and send to a single dest.
  if (destDevice != DeviceType::ALL) {
    addD2DSend(graphOpInfo, tensorShapeAttrIdx, transferId, destDevice);
    return;
  }

  // Insert a D2DSend for each dest device that's different from src.
  for (auto destDeviceForSend : deviceInfo.getUsedDeviceTypes()) {
    if (destDeviceForSend == srcDevice) {
      // When dest is src, update the mapping, and do not send.
      ValueMap[getSingleValueResult(inst)] = remapValue(inst->getOperand(0));
      continue;
    }
    addD2DSend(graphOpInfo, tensorShapeAttrIdx, transferId, destDeviceForSend);
  }
}

void DevicePartitionCloner::initBlock(SILBasicBlock *BB) {
  auto newBB = Builder.getFunction().createBasicBlock();
  BBMap[BB] = newBB;

  if (BB == srcFn.getEntryBlock()) {
    // If this is the entry block, for the primary function, add the parameter
    // BB arguments. Note helper functions do not take input tensors as
    // parameters, or return tensors.
    if (!isPrimaryFn()) return;

    for (auto arg : srcFn.getArguments()) {
      auto argTy = arg->getType();
      auto newArg = newBB->createFunctionArgument(argTy);
      ValueMap[arg] = SILValue(newArg);
    }
    return;
  }

  // For a non-entry basic block, if it has arguments, clone over any marked
  // ones.
  for (auto *arg : BB->getArguments()) {
    if (!shouldCloneArgument(arg))
      continue;

    // Create the argument and copy it into the ValueMap so future references
    // use it.
    ValueMap[arg] = newBB->createPHIArgument(remapType(arg->getType()),
                                             ValueOwnershipKind::Trivial,
                                             arg->getDecl());
  }
}

void DevicePartitionCloner::cloneBlock(SILBasicBlock *BB) {
  auto newBB = BBMap[BB];

  Builder.setInsertionPoint(newBB);
  for (auto &inst : *BB) {
    visit(&inst);
  }
}

void DevicePartitionCloner::cloneFunction() {
  // Go through and create all the blocks before we start cloning the
  // instructions over.  This allows us to remap instructions when we clone
  // them over.
  for (auto &BB : srcFn) {
    initBlock(&BB);
  }

  // Now that all the basic blocks and BBArguments are created, we can walk the
  // function in depth first order copying the code over.  Because we're working
  // in depth first order and have BB Arguments resolved, we're guaranteed to
  // see all definitions before uses.
  for (auto *BB : llvm::depth_first(&srcFn)) {
    cloneBlock(BB);
  }

  auto& resultFn = getBuilder().getFunction();
  if (auto *outs = getTFDumpIntermediateStream()) {
    *outs << "--- TFDevicePartition Per-Device Function Extraction Result: "
                 << resultFn.getName() << "\n";
    resultFn.print(*outs);
    *outs << "----\n";
    outs->flush();
  }

#ifndef NDEBUG
  // Verify that the extracted function is well-formed.
  resultFn.verify();
#endif
}

namespace swift {
namespace tf {

class DevicePartitionerImpl
    : public SILInstructionVisitor<DevicePartitionerImpl> {
  SILFunction &srcFn;
  const GraphFunctionDeviceInfo &deviceInfo;

  /// Tracks for each device type, the set of instructions that run on it.
  ///
  /// For the "ALL" pseudo-device, all instruction running on it also have
  /// mapping entries for each invidiual real device. The map entry key'ed on
  /// ALL is used to progagate the device requirement on a pipeline of
  /// instructions.  For example, we create an int interal on ALL devices, use
  /// it to create a Const tfop on ALL devices, and in turn use that const as
  /// the upper bound for loop iteration, where the loop runs on all devices.
  SmallPtrSet<SILInstruction *, 8> instByDevice[NUM_DEVICE_TYPES];

  /// When a SIL value v on some device D1 is to be consumed by an instruction
  /// on another device D2, prepare() below inserts a TensorTransfer instruction
  /// w to transfer v to device D2.
  ///
  /// A map entry is added accordingly for: (v, D2) -> w. This way if another
  /// instruction on device D2 also wants to consume v, we need not do another
  /// tensor transfer.
  using KeyByInstDestDevice =
      llvm::PointerIntPair<SILInstruction *, 3, unsigned>;
  llvm::DenseMap<KeyByInstDestDevice, SILValue> transferInstsByDestDevice;

  /// This is a counter we use to give each cross-device send/receive operation
  /// a unique ID.
  int nextTensorTransferId = 0;

 public:
  /// Impl note: Although we can short-circuit
  /// markFunctionAndInsertTensorTransfers() and extractFunctionForDevice() when
  /// there is a single device, we choose to exercise them for test
  /// coverage. This can be optimized for compiler performance later if it turns
  /// out to matter.
   DevicePartitionerImpl(SILFunction &srcFn,
                         const GraphFunctionDeviceInfo &deviceInfo)
       : srcFn(srcFn), deviceInfo(deviceInfo) {
     static_assert(
         NUM_DEVICE_TYPES <= 8,
         "3 bits are allocated in KeyByInstDestDevice to encode device types");
     markFunctionAndInsertTensorTransfers();
   }

  /// Returns a function extracted from `srcFn`, specialized on `deviceType`.
  ///
  /// For example, say `fn` returns a+b, where a and b and constant tensors,
  /// and a is placed on GPU.
  /// - The extracted function for GPU device has the constant node a, fed into
  ///   a _Send() node to CPU.
  /// - The extracted function for CPU device has _Recv node from GPU to read
  ///   a, and adds its output with const tensor b to produce the sum result.
  SILFunction *extractFunctionForDevice(DeviceType deviceType) {
    bool isPrimaryFn = deviceType == deviceInfo.primaryDeviceType;
    auto newFnType =
        isPrimaryFn ? srcFn.getLoweredFunctionType()
                    : SILFunctionType::get(
                          /*genericSig*/ nullptr, SILFunctionType::ExtInfo(),
                          SILCoroutineKind::None,
                          ParameterConvention::Direct_Owned, /*params*/ {},
                          /*interfaceYields*/ {},
                          /*results*/ {}, /*interfaceErrorResult*/ None,
                          srcFn.getModule().getASTContext());

    std::string resultFnName = srcFn.getName().str() + "_" +
                               getDeviceShortName(deviceType) +
                               ".device_partition";
    auto resultFn = srcFn.getModule().getOrCreateFunction(
        srcFn.getLocation(), resultFnName, SILLinkage::Private, newFnType,
        /*What's this*/ IsBare, IsNotTransparent, IsNotSerialized);

    DevicePartitionCloner PC(srcFn, deviceInfo, deviceType,
                             instByDevice[(unsigned)deviceType], *resultFn);

    // Fill in the cloned function body.
    PC.cloneFunction();

    // return fn;
    return resultFn;
  }

  void visitBuiltinInst(BuiltinInst *inst) {
    if (auto tfopInfo = SILTensorOpInfo::decode(inst))
      return visitTFOpInst(inst, tfopInfo.getValue());

    // For this magic builtin, it will be handled in graph lowering directly.
    if (inst->getName().str() == "tf_tensor_to_i1")
      return;

    inst->dump();
    llvm_unreachable(
        "DevicePartitionerImpl cannot mark this instruction yet\n");
  }

  void visitTFOpInst(BuiltinInst *inst, SILTensorOpInfo &tfopInfo) {
    auto deviceString = tfopInfo.getDeviceString();
    auto deviceType = getOpDeviceType(deviceString);
    markInstForDevice(deviceType, inst);

    // If any operand of `inst` is produced on another device, insert a
    // TensorTransfer inst if that has not been done yet.
    for (unsigned i = 0, e = inst->getNumOperands(); i != e; ++i) {
      makeOperandLocal(deviceType, inst, i);
    }
  }

  void visitGraphOperationInst(GraphOperationInst *inst) {
    // For this magic builtin, it will be handled in graph lowering directly.
    if (inst->getName().str() == "tf_tensor_to_i1")
      return;

    auto deviceType = GraphOperationInfo(inst).getDeviceType();
    markInstForDevice(deviceType, inst);

    // If any operand of `inst` is produced on another device, insert a
    // TensorTransfer inst if that has not been done yet.
    for (unsigned i = 0, e = inst->getNumOperands(); i != e; ++i) {
      makeOperandLocal(deviceType, inst, i);
    }
  }

  void visitReturnInst(ReturnInst *inst) {
    // The return is either using a single value or a tuple of values (which
    // could be empty).  These become the results of the graph.
    if (auto *ti = dyn_cast<TupleInst>(inst->getOperand())) {
      for (unsigned i = 0, e = ti->getNumOperands(); i != e; ++i) {
        makeOperandLocal(deviceInfo.primaryDeviceType, ti, i);
      }
    } else {
      makeOperandLocal(deviceInfo.primaryDeviceType, inst,
                       /*operandIdx*/ 0);
    }
  }

  void visitUncheckedRefCastInst(UncheckedRefCastInst *inst) {
    auto opValue = inst->getOperand();
    DeviceType operandDeviceType;
    if (isa<SILArgument>(opValue)) {
      // Func args only live on the primary device, while other BB args are
      // replicated on all devices.
      operandDeviceType = inst->getParent() == srcFn.getEntryBlock()
                              ? deviceInfo.primaryDeviceType
                              : DeviceType::ALL;
    } else {
      // Find an arbitrary device which hosts `opValue`, and do a tensor
      // transfer if needed.
      assert(opValue->getDefiningInstruction());
      operandDeviceType =
          getSomeDevicePlacement(opValue->getDefiningInstruction());
    }

    markInstForDevice(operandDeviceType, inst);
  }

  // Replicate these instructions on all devices, and graph lowering will make
  // sure to only lower the ones consumed by any tfops as operands.
  void visitIntegerLiteralInst(IntegerLiteralInst *inst) {
    markInstForAllDevices(inst);
  }
  void visitFloatLiteralInst(FloatLiteralInst *inst) {
    markInstForAllDevices(inst);
  }
  void visitMetatypeInst(MetatypeInst *inst) {
    markInstForAllDevices(inst);
  }
  void visitStringLiteralInst(StringLiteralInst *inst) {
    markInstForAllDevices(inst);
  }
  void visitTupleExtractInst(TupleExtractInst *inst) {
    markInstForAllDevices(inst);
  }
  void visitStructExtractInst(StructExtractInst *inst) {
    markInstForAllDevices(inst);
  }
  void visitFunctionRefInst(FunctionRefInst *inst) {
    markInstForAllDevices(inst);
  }

  void visitBranchInst(BranchInst *inst) {
    // Each BB arg of the branch needs to be replicated on ALL devices.
    for (unsigned i = 0, e = inst->getNumArgs(); i != e; ++i) {
      makeOperandLocal(DeviceType::ALL, inst, i);
    }
  }

  // This gets special handling in graph lowering.
  void visitCondBranchInst(CondBranchInst *inst) {}

  void visitTupleInst(TupleInst *inst) {
    // Tuples never exist in the graph except when they are the argument to
    // the return instruction, and will be handled there.
    assert(inst->hasOneUse() &&
           isa<ReturnInst>(inst->getSingleUse()->getUser()) &&
           "Unexpected tuple_inst in DevicePartitionerImpl");
  }

  // visitSILInstruction is the bottom level of the instruction visitor, where
  // unhandled instructions bottom out in.
  void visitSILInstruction(SILInstruction *inst) {
    inst->print(llvm::errs());
    llvm_unreachable(
        "DevicePartitionerImpl cannot lower this instruction yet\n");
  }

 private:
  /// Track the tensor ops on a per device basis, and insert "TensorTransfer"
  /// builtin's for cross-device TF tensor sends/recvs.
  void markFunctionAndInsertTensorTransfers() {
    for (auto *BB : llvm::depth_first(&srcFn)) {
      processBlock(BB);
    }

    if (auto *outs = getTFDumpIntermediateStream()) {
      *outs << "--- TFDevicePartition Cross Device Tensor Transfer Annotation "
               "Result: "
            << srcFn.getName() << "\n";
      srcFn.print(*outs);
      *outs << "----\n";
      outs->flush();
    }

#ifndef NDEBUG
  // Verify that the processed function is still well-formed.
  srcFn.verify();
#endif
  }

  void processBlock(SILBasicBlock *BB) {
    // Iterate carefully to avoid invalidating iterators: we may add more
    // instructions while we walk each basic block, and those new instructions
    // need not be processed/visited.
    for (auto iter = BB->begin(), e = BB->end(); iter != e;) {
      auto *inst = &*iter;
      ++iter;  // Increment the iterator in case we do no transformation.
      visit(inst);
    }
  }

  /// If the operand is produced on device, denoted by S, and S is different
  /// from `deviceType`, denoted by D, insert tensor transfer from S to D if
  /// that has not been done, and rewrite the operand to read from the
  /// transferred, local value.
  ///
  /// When we insert a TensorTransfer graph_op, S cannot be ALL, since otherwise
  /// we know the operand is already local to D.  Note D can be ALL though. In
  /// that case, when the device-specific function on D processes this
  /// TensorTransfer, it should send the operand to all devices but itself.
  void makeOperandLocal(DeviceType deviceType, SILInstruction *inst,
                        unsigned operIdx) {
    auto opValue = inst->getOperand(operIdx);

    // BB args are replicated on all devices, so no transfer is needed.
    if (isa<SILArgument>(opValue)) return;

    auto *operandInst = opValue->getDefiningInstruction();
    assert(operandInst && "value must be defined by an instruction");
    
    DeviceType operandDeviceType = getSomeDevicePlacement(operandInst);
    // Already on this device -- we are done.
    if (operandDeviceType == DeviceType::ALL ||
        instByDevice[(unsigned)deviceType].count(operandInst))
      return;
    // If `inst` runs on ALL devices but the graph only involves 1 device, can
    // also skip the send.
    if (deviceType == DeviceType::ALL && deviceInfo.numUsedDeviceTypes == 1)
      return;

    assert(operandDeviceType != DeviceType::ALL);
    assert(operandDeviceType != deviceType);
    // See if we have previously emitted a TransorTransfer from
    // `operandDeviceType` to `deviceType`.
    // FIXME: If we earlier sent the value to GPU device, and later lookup the
    // same value with dest device set to ALL, we will be generating another
    // send here, but it could be optimized away.
    auto lookupKey = KeyByInstDestDevice(operandInst, (unsigned)deviceType);
    if (!transferInstsByDestDevice.count(lookupKey)) {
      assert(isTensorFlowValue(opValue->getType()) &&
             "Can only transfer TensorFlow values");

      // Now we create a TensorTransfer graph_op inst. This inst cannot have its
      // src device set to ALL, but dest device can be ALL.
      assert(deviceInfo.numUsedDeviceTypes >= 2);

      // This inst has type:
      // <T> (T) {transferId$int, srcDevice$str, destDevice$str} -> T
      // Optionally, it also has a shape array attribute (needed for TPU).
      auto newInstName = std::string("tfc.TensorTransfer");
      newInstName +=
          GraphOperationInfo::getInputMarker(GraphOperationInfo::IM_Normal);

      auto loc = inst->getLoc();
      SILBuilder B(inst);
      auto &ctx = B.getASTContext();

      auto &allocator = ctx.getAllocator();
      SmallVector<GraphOperationAttribute, 4> attributes;
      attributes.push_back(
          {ctx.getIdentifier("transferId"),
           SymbolicValue::getInteger(nextTensorTransferId++, 32)});
      attributes.push_back(
          {ctx.getIdentifier("srcDevice"),
           SymbolicValue::getString(getDeviceString(operandDeviceType),
                                    allocator)});
      attributes.push_back(
          {ctx.getIdentifier("destDevice"),
           SymbolicValue::getString(getDeviceString(deviceType),
                                    allocator)});
      // The operand must have been produced by a graph_op inst, or an
      // UncheckedRefCastInst.
      if (auto *graphOpInst = dyn_cast<GraphOperationInst>(operandInst)) {
        for (unsigned i = 0, e = graphOpInst->getNumAttributes(); i != e; ++i) {
          auto attr = graphOpInst->getAttribute(i);
          auto attrInfo = GraphOperationInfo::decodeAttributeName(attr.name);
          if (!tf::isShapeArrayPseudoAttr(attrInfo.first, attr.value))
            continue;
          attributes.push_back(attr);
        }
      }

      auto *transferInst = B.createGraphOperation(
          loc, ctx.getIdentifier(newInstName),
          /*operands*/ {opValue}, attributes, {opValue->getType()});

      markInstForDevice(operandDeviceType, transferInst);
      markInstForDevice(deviceType, transferInst);
      transferInstsByDestDevice[lookupKey] = getSingleValueResult(transferInst);
    }
    inst->setOperand(operIdx, transferInstsByDestDevice[lookupKey]);
  }

  DeviceType getSomeDevicePlacement(SILInstruction* inst) const {
    assert(inst);
    // First check on the ALL pseudo-device, since an instruction running there
    // will guarantee to provide outputs to their use sites without having to do
    // tensor transfer.
    if (instByDevice[(unsigned)DeviceType::ALL].count(inst)) {
      return DeviceType::ALL;
    }

    Optional<DeviceType> operandDeviceType = None;
    for (auto type : deviceInfo.getUsedDeviceTypes()) {
      if (instByDevice[(unsigned)type].count(inst)) {
        operandDeviceType = type;
        break;
      }
    }
    if (!operandDeviceType.hasValue()) {
      inst->print(llvm::errs());
      llvm_unreachable(
          "The above instruction has not been marked for device placement: \n");
    }
    return operandDeviceType.getValue();
  }

  void markInstForDevice(DeviceType deviceType, SILInstruction *inst) {
    instByDevice[(unsigned)deviceType].insert(inst);
    if (deviceType == DeviceType::ALL) {
      markInstForAllDevices(inst);
    }
  }

  void markInstForAllDevices(SILInstruction *inst) {
    for (auto deviceType : deviceInfo.getUsedDeviceTypes()) {
      instByDevice[(unsigned)deviceType].insert(inst);
    }
    instByDevice[(unsigned)DeviceType::ALL].insert(inst);
  }
};

DevicePartitioner::DevicePartitioner(SILFunction &srcFn,
                                     const GraphFunctionDeviceInfo &deviceInfo)
    : impl(new DevicePartitionerImpl(srcFn, deviceInfo)) {}

DevicePartitioner::~DevicePartitioner() { delete impl; }

SILFunction *DevicePartitioner::extractFunctionForDevice(
    DeviceType deviceType) {
  return impl->extractFunctionForDevice(deviceType);
}

}  // end namespace tf
}  // end namespace swift

#endif  // SWIFT_ENABLE_TENSORFLOW
