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
#include "swift/SIL/GraphFunctionDeviceInfo.h"
#include "swift/SIL/GraphOperationBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/DepthFirstIterator.h"

using namespace swift;
using namespace tf;

//===----------------------------------------------------------------------===//
// Device Partitioning Utilities
//===----------------------------------------------------------------------===//

// Return the device attribute associated with `inst`, which is required to
// exist.
StringRef swift::tf::getDeviceString(const GraphOperationInfo &graphOpInfo) {
  auto attr = graphOpInfo.getInst()->getAttributeNamed(DEVICE_ATTR);
  assert(attr.hasValue() && "Tensor op instruction has no device string");
  return attr.getValue().getStringValue();
}

DeviceType swift::tf::getDeviceType(const GraphOperationInfo &graphOpInfo) {
  return getOpDeviceType(getDeviceString(graphOpInfo));
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

  /// A TensorTransfer graph_op sends a tensor value from a specific src TF
  /// device to one or ALL dest TF devices, where the src and dest devices must
  /// be different.
  /// For example, if tensor value x is produced on CPU, and has a use on GPU of
  /// form foo(x), we will insert a TensorTransfer graph_op so that the code
  /// becomes:
  ///   x = <inst defining x, running on CPU>
  ///   x' = TensorTransfer(x, ...)  // src is CPU, and dest is GPU
  ///   foo(x') // this runs on GPU
  ///
  /// This graph_op helps maintain the invariant that for any instruction I
  /// running on some device D, for any operand OP of I, OP must be present on D
  /// (either because OP is produced on D, or it is transferred via this
  /// graph_op).
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
  GraphOperationInfo decoder(inst);
  auto &structuredArguments = decoder.getStructuredArguments();
  if (decoder.getOperationName() == "tfc.TensorTransfer") {
    assert(structuredArguments.size() == 1);
    assert(structuredArguments[0].getKind() == GraphOperationInfo::SAK_Single);
    visitTensorTransferInst(decoder);
    return;
  }

  auto deviceType = getDeviceType(decoder);

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

void DevicePartitionCloner::addD2DSend(GraphOperationInfo &graphOpInfo,
                                       unsigned tensorShapeAttrIdx,
                                       int transferId, DeviceType destDevice) {
  auto srcDevice = thisDeviceType;
  assert(srcDevice != DeviceType::ALL);
  assert(destDevice != DeviceType::ALL);
  assert(srcDevice != destDevice);
  auto &B = getBuilder();
  auto &ctx = B.getASTContext();
  auto *inst = graphOpInfo.getInst();
  auto loc = remapLocation(getUserSourceLocation(inst->getDebugLocation()));

  // Insert a send inst, with type <T> (T) {int, str, str} -> ()
  GraphOperationBuilder newInstBuilder("tfc.D2DTensorSend");

  auto &allocator = ctx.getAllocator();
  newInstBuilder.addAttribute({ctx.getIdentifier("transferId"),
                        SymbolicValue::getInteger(transferId, 32)});
  newInstBuilder.addAttribute(
      {ctx.getIdentifier("destDevice"),
       SymbolicValue::getString(getDeviceString(destDevice), allocator)});
  newInstBuilder.addAttribute(
      {ctx.getIdentifier(DEVICE_ATTR),
       SymbolicValue::getString(getDeviceString(thisDeviceType), allocator)});

  auto valueToSend = remapValue(inst->getOperand(0));
  newInstBuilder.addArgument(valueToSend);
  if (inst->getNumAttributes() > tensorShapeAttrIdx)
    newInstBuilder.addAttribute(inst->getAttribute(tensorShapeAttrIdx));
  newInstBuilder.build(B, ctx, loc, {});
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
  auto *inst = graphOpInfo.getInst();
  auto loc = remapLocation(getUserSourceLocation(inst->getDebugLocation()));

  // Insert a recv inst, with type <T> {int, str, str} -> (T)
  GraphOperationBuilder newInstBuilder("tfc.D2DTensorRecv");

  auto &allocator = ctx.getAllocator();
  SmallVector<GraphOperationAttribute, 4> attributes;
  newInstBuilder.addAttribute({ctx.getIdentifier("transferId"),
                        SymbolicValue::getInteger(transferId, 32)});
  newInstBuilder.addAttribute(
      {ctx.getIdentifier("srcDevice"),
       SymbolicValue::getString(getDeviceString(srcDevice), allocator)});
  newInstBuilder.addAttribute(
      {ctx.getIdentifier(DEVICE_ATTR),
       SymbolicValue::getString(getDeviceString(thisDeviceType), allocator)});

  auto valueTy = inst->getResults()[0]->getType();
  if (inst->getNumAttributes() > tensorShapeAttrIdx)
    newInstBuilder.addAttribute(inst->getAttribute(tensorShapeAttrIdx));

  auto valueToRecv = getSingleValueResult(inst);
  auto *transferInst = newInstBuilder.build(B, ctx, loc, {valueTy});
  auto newValue = getSingleValueResult(transferInst);
  ValueMap[valueToRecv] = newValue;
}

void DevicePartitionCloner::visitTensorTransferInst(
    GraphOperationInfo &graphOpInfo) {
  auto *inst = graphOpInfo.getInst();
  assert(inst->getNumResults() == 1);
  assert(inst->getNumOperands() == 1);
  assert(inst->getNumAttributes() == 3 || inst->getNumAttributes() == 4);

  int transferId = graphOpInfo.getIntAttr(0, "transferId");
  auto srcDeviceStr = graphOpInfo.getStringAttr(1, "srcDevice");
  auto srcDevice = getOpDeviceType(srcDeviceStr);
  auto destDeviceStr = graphOpInfo.getStringAttr(2, "destDevice");
  auto destDevice = getOpDeviceType(destDeviceStr);
  assert(srcDevice != destDevice);
  // This graph_op cannot have src device set to ALL, but dest device can be ALL.
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
    ValueMap[arg] = newBB->createPhiArgument(remapType(arg->getType()),
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
  SILTransform &parentTransform;
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
  int &nextTensorTransferId;

public:
  /// Impl note: Although we can short-circuit
  /// markFunctionAndInsertTensorTransfers() and extractFunctionForDevice() when
  /// there is a single device, we choose to exercise them for test
  /// coverage. This can be optimized for compiler performance later if it turns
  /// out to matter.
  DevicePartitionerImpl(SILTransform &parentTransform,
                        SILFunction &srcFn,
                        const GraphFunctionDeviceInfo &deviceInfo,
                        int &nextTensorTransferId)
      : parentTransform(parentTransform), srcFn(srcFn), deviceInfo(deviceInfo),
        nextTensorTransferId(nextTensorTransferId) {
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
    SILOptFunctionBuilder FB(parentTransform);
    auto resultFn = FB.getOrCreateFunction(
        srcFn.getLocation(), resultFnName, SILLinkage::Private, newFnType,
        /*What's this*/ IsBare, IsNotTransparent, IsNotSerialized);

    DevicePartitionCloner PC(srcFn, deviceInfo, deviceType,
                             instByDevice[(unsigned)deviceType], *resultFn);

    // Fill in the cloned function body.
    PC.cloneFunction();

    // return fn;
    return resultFn;
  }

  void visitGraphOperationInst(GraphOperationInst *inst) {
    auto deviceType = getDeviceType(GraphOperationInfo(inst));
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
  /// graph_op's for cross-device TF tensor sends/recvs.
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

    // Undefs will be handled later.
    if (isa<SILUndef>(opValue)) return;
    if (isa<SILArgument>(opValue)) {
      // BB args that are not func input args are replicated on all devices, so
      // no transfer is needed.
      auto *funcArg = dyn_cast<SILFunctionArgument>(opValue);
      if (!funcArg)
        return;

      // Otherwise, `inst` can look like like:
      //   %3 = graph_op "tf_tensor_to_i1"(%0 : $TensorHandle<Builtin.Int1>)
      //     {__device: "ALL_DEVICES"} : $Builtin.Int1
      // Here we need to transfer %0 to the device that runs the graph_op. To
      // keep the remaining code in this function simple (the code assumes
      // `opValue` is produced by a graph_op inst), we synthesize an identity op
      // that reads the func input arg at the primary device. If it turns out
      // reading the func arg need no tensor transfer (i.e., if the graph_op %3
      // above is placed on the primary device), we rely on the backend graph
      // compiler (e.g. grappler) to optimize away this extraneous identity op.
      GraphOperationBuilder identityOpBuilder("Identity");
      identityOpBuilder.addArgument(opValue);

      // insert this new inst at the beginning of the function.
      SILBuilder B(&srcFn.front().front());
      auto &ctx = B.getModule().getASTContext();
      identityOpBuilder.addAttribute({
          ctx.getIdentifier(DEVICE_ATTR),
          SymbolicValue::getString(
              getDeviceString(deviceInfo.primaryDeviceType),
              ctx.getAllocator())});
      auto *identityOpInst = identityOpBuilder.build(
          B, ctx, srcFn.getLocation(), {opValue->getType()});
      markInstForDevice(deviceInfo.primaryDeviceType, identityOpInst);

      auto newValue = getSingleValueResult(identityOpInst);
      // Replace all users with the value produced by the new identity op inst,
      // except for the identity inst itself.
      for (auto *use : opValue->getUses()) {
        auto *user = use->getUser();
        if (user != identityOpInst)
          use->set(newValue);
      }
      opValue = newValue;
    }

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
    // See if we have previously emitted a TensorTransfer from
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
      GraphOperationBuilder newInstBuilder("tfc.TensorTransfer");
      newInstBuilder.addArgument(opValue);

      auto loc = inst->getLoc();
      // Insert the transfer right after the operandInst.
      SILBuilder B(std::next(operandInst->getIterator()));
      auto &ctx = B.getASTContext();

      auto &allocator = ctx.getAllocator();
      newInstBuilder.addAttribute(
          {ctx.getIdentifier("transferId"),
           SymbolicValue::getInteger(nextTensorTransferId++, 32)});
      newInstBuilder.addAttribute(
          {ctx.getIdentifier("srcDevice"),
           SymbolicValue::getString(getDeviceString(operandDeviceType),
                                    allocator)});
      newInstBuilder.addAttribute(
          {ctx.getIdentifier("destDevice"),
           SymbolicValue::getString(getDeviceString(deviceType),
                                    allocator)});
      // The operand must have been produced by a graph_op inst, or an
      // UncheckedRefCastInst.
      if (auto *graphOpInst = dyn_cast<GraphOperationInst>(operandInst)) {
        for (unsigned i = 0, e = graphOpInst->getNumAttributes(); i != e; ++i) {
          auto attr = graphOpInst->getAttribute(i);
          auto attrInfo = GraphOperationInfo::decodeArgumentName(attr.name.str());
          if (!tf::isShapeArrayPseudoAttr(attrInfo.first, attr.value))
            continue;
          newInstBuilder.addAttribute(attr);
        }
      }

      auto *transferInst = newInstBuilder.build(B, ctx, loc,
                                                {opValue->getType()});

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

DevicePartitioner::DevicePartitioner(SILTransform &parentTransform,
                                     SILFunction &srcFn,
                                     const GraphFunctionDeviceInfo &deviceInfo,
                                     int &nextTensorTransferId)
    : impl(new DevicePartitionerImpl(parentTransform, srcFn, deviceInfo,
                                     nextTensorTransferId)) {
}

DevicePartitioner::~DevicePartitioner() { delete impl; }

SILFunction *DevicePartitioner::extractFunctionForDevice(
    DeviceType deviceType) {
  return impl->extractFunctionForDevice(deviceType);
}

}  // end namespace tf
}  // end namespace swift

#endif  // SWIFT_ENABLE_TENSORFLOW
