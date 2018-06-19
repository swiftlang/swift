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

#include "TFUtilities.h"
#ifdef SWIFT_ENABLE_TENSORFLOW
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/ADT/DepthFirstIterator.h"

using namespace swift;
using namespace tf;

namespace {

/// Extracts from an input accelerator SIL function a device-specific SIL
/// function.
/// This class is adapted from PartitionCloner.
class DevicePartitionCloner
    : public SILClonerWithScopes<DevicePartitionCloner> {
  SILFunction& srcFn;

  const GraphGlobalConfiguration &configuration;

  /// The device for us to extract the SIL computation in.
  const DeviceType thisDeviceType;

  /// The set of ops in `srcFn` to run on `thisDeviceType`.
  const SmallPtrSetImpl<SILInstruction *> &targetOps;

 public:
  DevicePartitionCloner(SILFunction &srcFn,
                        const GraphGlobalConfiguration &configuration,
                        DeviceType thisDeviceType,
                        const SmallPtrSetImpl<SILInstruction *> &targetOps,
                        SILFunction &NewFn)
      : SILClonerWithScopes(NewFn),
        srcFn(srcFn),
        configuration(configuration),
        thisDeviceType(thisDeviceType),
        targetOps(targetOps) {
    assert(thisDeviceType != DeviceType::ALL);
  }

  /// Extracts the device-specific computation into `NewFn` above.
  void cloneFunction();

  void visitTFOpInst(BuiltinInst *inst, SILTensorOpInfo &tfopInfo);

  void visitBuiltinInst(BuiltinInst *inst) {
    if (auto tfopInfo = SILTensorOpInfo::decode(inst)) {
      if (targetOps.count(inst))
        visitTFOpInst(cast<BuiltinInst>(inst), tfopInfo.getValue());
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
    return configuration.deviceType == thisDeviceType;
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
  void visitTensorTransferInst(BuiltinInst *inst, SILTensorOpInfo &tfopInfo);
  void addD2DSend(BuiltinInst *inst, int transferId, DeviceType destDevice);
  void addD2DRecv(BuiltinInst *inst, int transferId, DeviceType srcDevice);

  void initBlock(SILBasicBlock *BB);

  /// Move and clone code over from the input block into this block, inserting
  /// transfers between the host and destination code as necessary.
  void cloneBlock(SILBasicBlock *BB);
};
}  // end anonymous namespace

void DevicePartitionCloner::visitTFOpInst(BuiltinInst *inst,
                                          SILTensorOpInfo &tfopInfo) {
  if (tfopInfo.opName == "tfc.TensorTransfer") {
    visitTensorTransferInst(inst, tfopInfo);
    return;
  }

  auto deviceString = tfopInfo.getDeviceString();
  auto deviceType = OpDeviceType(deviceString);

  bool shouldRunInstOnThisDevice =
      deviceType == DeviceType::ALL || deviceType == thisDeviceType;
  if (!shouldRunInstOnThisDevice) {
    // Skip this inst
    return;
  }

  auto &B = getBuilder();
  auto loc = remapLocation(getUserSourceLocation(inst->getDebugLocation()));

  // Clone it over.
  SmallVector<SILValue, 4> args;
  auto cloneSingleInst =
      [&](SingleValueInstruction *inst) -> SingleValueInstruction * {
    auto ourInst = inst->clone();
    ourInst->setDebugLocation(B.getSILDebugLocation(loc));
    B.getInsertionBB()->push_back(ourInst);
    return ourInst;
  };

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

void DevicePartitionCloner::addD2DSend(BuiltinInst *inst, int transferId,
                                       DeviceType destDevice) {
  auto srcDevice = thisDeviceType;
  assert(srcDevice != DeviceType::ALL);
  assert(destDevice != DeviceType::ALL);
  assert(srcDevice != destDevice);
  auto &B = getBuilder();
  auto &ctx = B.getASTContext();
  auto loc = remapLocation(getUserSourceLocation(inst->getDebugLocation()));
  auto transferIdAttr = B.createIntegerLiteral(
      loc, SILType::getBuiltinIntegerType(32, ctx), transferId);
  auto deviceAttr =
      B.createStringLiteral(loc, StringRef(getDeviceString(thisDeviceType)),
                            StringLiteralInst::Encoding::UTF8);

  // Insert a send inst, with type <T> (T, int, str, str) -> ()
  auto newInstName =
      "__tfop_tfc.D2DTensorSend,$in,transferId,destDevice,device";

  auto voidTy = B.getModule().Types.getEmptyTupleType();
  auto valueToSend = remapValue(inst->getOperand(0));
  auto destDeviceStr = getDeviceString(destDevice);
  auto valueTy = inst->getResults()[0]->getType();
  auto destDeviceAttr = B.createStringLiteral(
      loc, StringRef(destDeviceStr), StringLiteralInst::Encoding::UTF8);
  B.createBuiltin(loc, ctx.getIdentifier(newInstName), voidTy,
                  /*MERGE*/ inst->getSubstitutions(),
                  {valueToSend, transferIdAttr, destDeviceAttr, deviceAttr});
  // Do not update ValueMap since Send does not produce a value.
}

void DevicePartitionCloner::addD2DRecv(BuiltinInst *inst, int transferId,
                                       DeviceType srcDevice) {
  auto destDevice = thisDeviceType;
  assert(srcDevice != DeviceType::ALL);
  assert(destDevice != DeviceType::ALL);
  assert(srcDevice != destDevice);
  auto &B = getBuilder();
  auto &ctx = B.getASTContext();
  auto loc = remapLocation(getUserSourceLocation(inst->getDebugLocation()));
  auto transferIdAttr = B.createIntegerLiteral(
      loc, SILType::getBuiltinIntegerType(32, ctx), transferId);
  auto deviceAttr =
      B.createStringLiteral(loc, StringRef(getDeviceString(thisDeviceType)),
                            StringLiteralInst::Encoding::UTF8);

  // Insert a recv inst, with type <T> (int, str, str) -> (T)
  auto newInstName = "__tfop_tfc.D2DTensorRecv,transferId,srcDevice,device";

  auto srcDeviceStr = getDeviceString(srcDevice);
  auto srcDeviceAttr = B.createStringLiteral(loc, StringRef(srcDeviceStr),
                                             StringLiteralInst::Encoding::UTF8);
  auto valueTy = inst->getResults()[0]->getType();
  auto newValue =
      B.createBuiltin(inst->getLoc(), ctx.getIdentifier(newInstName), valueTy,
                      inst->getSubstitutions(),
                      {transferIdAttr, srcDeviceAttr, deviceAttr});
  auto valueToRecv = inst->getResults()[0];
  ValueMap[valueToRecv] = newValue;
}

void DevicePartitionCloner::visitTensorTransferInst(BuiltinInst *inst,
                                                    SILTensorOpInfo &tfopInfo) {
  assert(inst->getNumResults() == 1);
  assert(inst->getNumOperands() == 4);
  assert(tfopInfo.isInput(0));

  int transferId = tfopInfo.getIntAttrOperand(1, "transferId");
  auto srcDeviceStr = tfopInfo.getStringAttrOperand(2, "srcDevice");
  auto srcDevice = OpDeviceType(srcDeviceStr);
  auto destDeviceStr = tfopInfo.getStringAttrOperand(3, "destDevice");
  auto destDevice = OpDeviceType(destDeviceStr);
  assert(srcDevice != destDevice);
  // This builtin cannot have src device set to ALL, but dest device can be ALL.
  assert(srcDevice != DeviceType::ALL);
  bool shouldRunTransferAsSrcDevice = srcDevice == thisDeviceType;
  bool shouldRunTransferAsDestDevice =
      destDevice == DeviceType::ALL || destDevice == thisDeviceType;
  if (!shouldRunTransferAsSrcDevice && !shouldRunTransferAsDestDevice) return;

  if (!shouldRunTransferAsSrcDevice) {
    assert(shouldRunTransferAsDestDevice);
    addD2DRecv(inst, transferId, srcDevice);
    return;
  }

  // Run transfer as src device, and send to a single dest.
  if (destDevice != DeviceType::ALL) {
    addD2DSend(inst, transferId, destDevice);
    return;
  }

  // Insert a D2DSend for each dest device that's different from src.
  for (auto destDeviceForSend : configuration.usedDeviceTypes) {
    if (destDeviceForSend == srcDevice) {
      // When dest is src, update the mapping, and do not send.
      ValueMap[inst] = remapValue(inst->getOperand(0));
      continue;
    }
    addD2DSend(inst, transferId, destDeviceForSend);
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
  const GraphGlobalConfiguration &configuration;

  /// Maps a device type to the set of instructions that run on it.
  ///
  /// For the "ALL" pseudo-device, all instruction running on it also have
  /// mapping entries for each invidiual real device. The map entry key'ed on
  /// ALL is used to progagate the device requirement on a pipeline of
  /// instructions.  For example, we create an int interal on ALL devices, use
  /// it to create a Const tfop on ALL devices, and in turn use that const as
  /// the upper bound for loop iteration, where the loop runs on all devices.
  std::vector<SmallPtrSet<SILInstruction *, 8>> instByDevice;

  /// When a SIL value v on some device D1 is to be consumed by an instruction
  /// on another device D2, prepare() below inserts a TensorTransfer instruction
  /// w to transfer v to device D2.
  ///
  /// A map entry is added accordingly for: (v, D2) -> w. This way if another
  /// instruction on device D2 also wants to consume v, we need not do another
  /// tensor transfer.
  using KeyByInstDestDevice =
      llvm::PointerIntPair<SILInstruction *, 3, unsigned>;
  llvm::DenseMap<KeyByInstDestDevice, BuiltinInst *> transferInstsByDestDevice;

  /// This is a counter we use to give each cross-device send/receive operation
  /// a unique ID.
  int nextTensorTransferId = 0;

 public:
  /// Impl note: Although we can short-circuit
  /// markFunctionAndInsertTensorTranfers() and extractFunctionForDevice() when
  /// there is a single device, we choose to exercise them for test
  /// coverage. This can be optimized for compiler performance later if it turns
  /// out to matter.
  DevicePartitionerImpl(SILFunction &srcFn,
                   const GraphGlobalConfiguration &configuration)
    : srcFn(srcFn), configuration(configuration) {
    static_assert(
        NUM_DEVICE_TYPES <= 8,
        "3 bits are allocated in KeyByInstDestDevice to encode device types");
    instByDevice.resize(NUM_DEVICE_TYPES);
    markFunctionAndInsertTensorTranfers();
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
    bool isPrimaryFn = deviceType == configuration.deviceType;
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

    DevicePartitionCloner PC(srcFn, configuration, deviceType,
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

    inst->print(llvm::errs());
    llvm_unreachable(
        "DevicePartitionerImpl cannot mark this instruction yet\n");
  }

  void visitTFOpInst(BuiltinInst *inst, SILTensorOpInfo &tfopInfo) {
    auto deviceString = tfopInfo.getDeviceString();
    auto deviceType = OpDeviceType(deviceString);
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
        makeOperandLocal(configuration.deviceType, ti, i);
      }
    } else {
      makeOperandLocal(configuration.deviceType, inst,
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
                              ? configuration.deviceType
                              : DeviceType::ALL;
    } else {
      // Find an arbitrary device which hosts `opValue`, and do a tensor
      // transfer if needed.
      assert(isa<SILInstruction>((SILNode *)opValue));
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
  void markFunctionAndInsertTensorTranfers() {
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
  /// When we insert a TensorTransfer builtin, S cannot be ALL, since otherwise
  /// we know the operand is already local to D.  Note D can be ALL though. In
  /// that case, when the device-specifc function on D processes this
  /// TensorTransfer, it should send the operand to all devices but itself.
  void makeOperandLocal(DeviceType deviceType, SILInstruction *inst,
                        unsigned operIdx) {
    auto opValue = inst->getOperand(operIdx);

    // BB args are replicated on all devices, so no transfer is needed.
    if (isa<SILArgument>(opValue)) return;

    auto *operandInst = cast<SILInstruction>((SILNode *)opValue);
    DeviceType operandDeviceType = getSomeDevicePlacement(operandInst);
    // Already on this device -- we are done.
    if (operandDeviceType == DeviceType::ALL ||
        instByDevice[(unsigned)deviceType].count(operandInst))
      return;
    // If `inst` runs on ALL devices but the graph only involves 1 device, can
    // also skip the send.
    if (deviceType == DeviceType::ALL &&
        configuration.usedDeviceTypes.size() == 1)
      return;

    assert(operandDeviceType != DeviceType::ALL);
    assert(operandDeviceType != deviceType);
    // See if we have previously emitted a TransorTransfer from
    // `operandDeviceType` to `deviceType`.
    // FIXME: If we earlier sent the value to GPU device, and later lookup the
    // same value with dest device set to ALL, we will be generating another
    // send here, but it could be optimized away.
    auto lookupKey = KeyByInstDestDevice(operandInst, (unsigned)deviceType);
    auto findIt = transferInstsByDestDevice.find(lookupKey);
    if (findIt == transferInstsByDestDevice.end()) {
      assert(isTensorFlowValue(opValue->getType()) &&
             "Can only transfer TensorFlow values");

      // This builtin has type:
      // <T> (T, transferId$int, srcDevice$str, destDevice$str) -> T
      auto newInstName = std::string("__tfop_tfc.TensorTransfer,") +
                         "$in,transferId,srcDevice,destDevice";

      // Now we create a TensorTransfer builtin. This builtin cannot have its
      // src device set to ALL, but dest device can be ALL.
      assert(configuration.usedDeviceTypes.size() >= 2);

      auto loc = inst->getLoc();
      SILBuilder B(inst);
      auto &ctx = B.getASTContext();
      auto transferIdAttr = B.createIntegerLiteral(
          loc, SILType::getBuiltinIntegerType(32, ctx), nextTensorTransferId++);
      markInstForAllDevices(transferIdAttr);
      auto srcDeviceAttr = B.createStringLiteral(
          loc, StringRef(getDeviceString(operandDeviceType)),
          StringLiteralInst::Encoding::UTF8);
      markInstForAllDevices(srcDeviceAttr);
      auto destDeviceAttr =
          B.createStringLiteral(loc, StringRef(getDeviceString(deviceType)),
                                StringLiteralInst::Encoding::UTF8);
      markInstForAllDevices(destDeviceAttr);

      // At this point, the operand must have been produced by a `builtin` inst.
      auto builtinInst = cast<BuiltinInst>(operandInst);
      auto transferInst = B.createBuiltin(
          loc, ctx.getIdentifier(newInstName), opValue->getType(),
          builtinInst->getSubstitutions(),
          {opValue, transferIdAttr, srcDeviceAttr, destDeviceAttr});
      transferInstsByDestDevice[lookupKey] = transferInst;
      markInstForDevice(operandDeviceType, transferInst);
      markInstForDevice(deviceType, transferInst);
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
    for (auto type : configuration.usedDeviceTypes) {
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
    for (auto deviceType : configuration.usedDeviceTypes) {
      instByDevice[(unsigned)deviceType].insert(inst);
    }
    instByDevice[(unsigned)DeviceType::ALL].insert(inst);
  }
};

DevicePartitioner::DevicePartitioner(
    SILFunction &srcFn, const GraphGlobalConfiguration &configuration)
    : impl(new DevicePartitionerImpl(srcFn, configuration)) {}

DevicePartitioner::~DevicePartitioner() { delete impl; }

SILFunction *DevicePartitioner::extractFunctionForDevice(
    DeviceType deviceType) {
  return impl->extractFunctionForDevice(deviceType);
}

}  // end namespace tf
}  // end namespace swift

#endif  // SWIFT_ENABLE_TENSORFLOW
