//===--- AddressLowering.cpp - Lower SIL address-only types. --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// This pass lowers SILTypes. On completion, the SILType of every SILValue is
// its SIL storage type. A SIL storage type is always an address type for values
// that require indirect storage at the LLVM IR level. Consequently, this pass
// is required for IRGen. It is a mandatory IRGen preparation pass (not a
// diagnostic pass).
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "address-lowering"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using llvm::SmallSetVector;

//===----------------------------------------------------------------------===//
// ValueStorageMap: Map Opaque/Resilient SILValues to abstract storage units.
//===----------------------------------------------------------------------===//

namespace {
struct ValueStorage {
  // .None if this values requires its own allocation.
  // .Some if it is a projection from a larger storage unit.
  Optional<SILValue> projectionFrom;
  // The final address of this storage unit after rewriting the SIL.
  SILValue storageAddress;

  bool isProjection() const { return projectionFrom.hasValue(); }
};

/// Map each opaque/resilient SILValue to its abstract storage.
/// O(1) membership test.
/// O(n) iteration in RPO order.
class ValueStorageMap {
  typedef std::vector<std::pair<SILValue, ValueStorage>> ValueVector;
  // Hash of values to ValueVector indices.
  typedef llvm::DenseMap<SILValue, unsigned> ValueHashMap;

  ValueVector valueVector;
  ValueHashMap valueHashMap;

public:
  bool empty() const { return valueVector.empty(); }

  void clear() {
    valueVector.clear();
    valueHashMap.clear();
  }

  ValueVector::iterator begin() { return valueVector.begin(); }

  ValueVector::iterator end() { return valueVector.end(); }

  ValueVector::reverse_iterator rbegin() { return valueVector.rbegin(); }

  ValueVector::reverse_iterator rend() { return valueVector.rend(); }

  bool contains(SILValue value) const {
    return valueHashMap.find(value) != valueHashMap.end();
  }

  ValueStorage &insertValue(SILValue value) {
    auto hashResult =
        valueHashMap.insert(std::make_pair(value, valueVector.size()));
    assert(hashResult.second && "SILValue already mapped");

    valueVector.emplace_back(value, ValueStorage());

    return valueVector.back().second;
  }

  ValueStorage &getStorage(SILValue value) {
    auto hashIter = valueHashMap.find(value);
    assert(hashIter != valueHashMap.end() && "Missing SILValue");
    return valueVector[hashIter->second].second;
  }
};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// AddressLoweringState: shared state for the pass's analysis and transforms.
//===----------------------------------------------------------------------===//

namespace {
struct AddressLoweringState {
  SILFunction *F;
  // All opaque values and associated storage.
  ValueStorageMap valueStorageMap;
  // All direct operands with formally indirect SILArgument conventions.
  SmallVector<Operand *, 16> indirectOperands;
  // All call instruction's with formally indirect result conventions.
  SmallVector<SILInstruction *, 16> indirectResults;
  // Delete these instructions after performing transformations.
  // They must not have any remaining users.
  SmallSetVector<SILInstruction *, 16> instsToDelete;

  AddressLoweringState(SILFunction *F) : F(F) {}
};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// OpaqueValueVisitor: Map OpaqueValues to ValueStorage.
//===----------------------------------------------------------------------===//

namespace {
/// Collect all Opaque/Resilient values, inserting them in a ValueStorageMap in
/// RPO order.
///
/// Collect all call arguments with formally indirect SIL argument convention in
/// `indirectOperands` and formally indirect SIL results in `indirectResults`.
///
/// TODO: Perform linear-scan style in-place stack slot coloring by keeping
/// track of each value's last use.
class OpaqueValueVisitor {
  AddressLoweringState &pass;
  PostOrderFunctionInfo postorderInfo;

public:
  explicit OpaqueValueVisitor(AddressLoweringState &pass)
      : pass(pass), postorderInfo(pass.F) {}

  void mapValueStorage();

protected:
  void visitApply(ApplySite applySite);
  void visitValue(SILValue value);
};
} // end anonymous namespace

/// Populate valueStorageMap. Find all Opaque/Resilient SILValues and add them
/// to valueStorageMap in RPO.
void OpaqueValueVisitor::mapValueStorage() {
  for (auto *BB : postorderInfo.getReversePostOrder()) {
    // Opaque function arguments have already been replaced.
    if (BB != pass.F->getEntryBlock()) {
      for (auto argI = BB->args_begin(), argEnd = BB->args_end();
           argI != argEnd; ++argI) {
        visitValue(*argI);
      }
    }
    for (auto &II : *BB) {
      if (ApplySite::isa(&II))
        visitApply(ApplySite(&II));

      if (II.hasValue())
        visitValue(SILValue(&II));
    }
  }
}

/// Populate indirectOperands. Add an operand for each call argument with a
/// formally indirect convention.
void OpaqueValueVisitor::visitApply(ApplySite applySite) {
  if (applySite.getSubstCalleeType()->hasIndirectFormalResults())
    pass.indirectResults.push_back(applySite.getInstruction());

  auto calleeConv = applySite.getSubstCalleeConv();
  unsigned calleeArgIdx = applySite.getCalleeArgIndexOfFirstAppliedArg();
  for (Operand &operand : applySite.getArgumentOperands()) {
    if (operand.get()->getType().isObject()) {
      auto argConv = calleeConv.getSILArgumentConvention(calleeArgIdx);
      if (argConv.isIndirectConvention()) {
        pass.indirectOperands.push_back(&operand);
      }
    }
    ++calleeArgIdx;
  }
}

/// TODO: Set ValueStorage.projectionFrom whenever there is a single
/// isComposingOperand() use and the aggregate's storage can be hoisted to a
/// dominating point.
void OpaqueValueVisitor::visitValue(SILValue value) {
  if (value->getType().isAddressOnly(pass.F->getModule())) {
    if (pass.valueStorageMap.contains(value)) {
      assert(isa<SILFunctionArgument>(
          pass.valueStorageMap.getStorage(value).storageAddress));
      return;
    }
    pass.valueStorageMap.insertValue(value);
  }
}

//===----------------------------------------------------------------------===//
// OpaqueStorageAllocation: Generate alloc_stack and address projections for all
// abstract storage locations.
//===----------------------------------------------------------------------===//

namespace {
/// Replace indirect parameter arguments of this function with address-type
/// arguments.
///
/// Insert new indirect result arguments for this function to represent the
/// caller's storage.
///
/// Allocate storage on the stack for every opaque value defined in this
/// function in RPO order. If the definition is an argument of this function,
/// simply replace the function argument with an address representing the
/// caller's storage.
///
/// TODO: shrink lifetimes by inserting alloc_stack at the dominance LCA and
/// finding the lifetime boundary with a simple backward walk from uses.
class OpaqueStorageAllocation {
  AddressLoweringState &pass;

  SILBuilder allocBuilder;

public:
  explicit OpaqueStorageAllocation(AddressLoweringState &pass)
      : pass(pass), allocBuilder(*pass.F) {}

  void allocateOpaqueStorage();

protected:
  void replaceFunctionArgs();
  unsigned insertIndirectResultParameters(unsigned argIdx, SILType resultType);
  void allocateForValue(SILValue value, ValueStorage &storage);
  void allocateForOperand(Operand *operand);
  void allocateForResults(SILInstruction *origInst);
};
} // end anonymous namespace

/// Top-level entry point: allocate storage for all opaque/resilient values.
void OpaqueStorageAllocation::allocateOpaqueStorage() {
  auto canFnType = pass.F->getLoweredFunctionType();
  auto fnConv = pass.F->getConventions();

  // Setup a generic context for argument and result types.
  swift::Lowering::GenericContextScope scope(pass.F->getModule().Types,
                                             canFnType->getGenericSignature());

  // Fixup this function's argument types with temporary loads.
  replaceFunctionArgs();

  // Create a new function argument for each indirect result.
  auto contextualResultType =
      pass.F->mapTypeIntoContext(fnConv.getSILResultType());
  unsigned numIndirectResults =
      insertIndirectResultParameters(0, contextualResultType);

#ifndef NDEBUG
  SILFunctionConventions loweredFnConv(
      canFnType, SILModuleConventions::getLoweredAddressConventions());
  assert(numIndirectResults == loweredFnConv.getNumIndirectSILResults());
#endif
  (void)numIndirectResults;

  // Populate valueStorageMap.
  OpaqueValueVisitor(pass).mapValueStorage();

  // Find an insertion point for new AllocStack instructions.
  SILBasicBlock::iterator insertionPoint = pass.F->begin()->begin();
  while (isa<AllocStackInst>(*insertionPoint))
    ++insertionPoint;

  allocBuilder.setInsertionPoint(insertionPoint);

  // Create an AllocStack for every opaque value defined in the function.
  for (auto &valueStorageI : pass.valueStorageMap)
    allocateForValue(valueStorageI.first, valueStorageI.second);

  // Create an AllocStack for every formally indirect argument.
  for (Operand *operand : pass.indirectOperands)
    allocateForOperand(operand);

  // Create an AllocStack for every formally indirect result.
  for (SILInstruction *callInst : pass.indirectResults)
    allocateForResults(callInst);
}

/// Replace each value-typed argument with an address-typed argument by
/// inserting a temprorary load instruction.
void OpaqueStorageAllocation::replaceFunctionArgs() {
  // Insert temporary argument loads at the top of the function.
  allocBuilder.setInsertionPoint(pass.F->getEntryBlock()->begin());

  auto fnConv = pass.F->getConventions();
  unsigned argIdx = fnConv.getSILArgIndexOfFirstParam();
  for (SILParameterInfo param :
       pass.F->getLoweredFunctionType()->getParameters()) {

    if (param.isFormalIndirect()) {
      SILArgument *arg = pass.F->getArgument(argIdx);
      SILType addrType = arg->getType().getAddressType();

      LoadInst *loadArg = allocBuilder.createLoad(
          RegularLocation(const_cast<ValueDecl *>(arg->getDecl())),
          SILUndef::get(addrType, pass.F->getModule()),
          LoadOwnershipQualifier::Unqualified);

      arg->replaceAllUsesWith(loadArg);
      assert(!pass.valueStorageMap.contains(arg));

      arg = arg->getParent()->replaceFunctionArgument(
          arg->getIndex(), addrType, ValueOwnershipKind::Trivial,
          arg->getDecl());

      loadArg->setOperand(arg);

      pass.valueStorageMap.insertValue(loadArg).storageAddress = arg;
    }
    ++argIdx;
  }
  assert(argIdx
         == fnConv.getSILArgIndexOfFirstParam() + fnConv.getNumSILArguments());
}

/// Recursively insert function arguments for any @out result type at the
/// specified index. Return the index after the last inserted result argument.
///
/// This is effectively moved from the old SILGen emitIndirectResultParameters.
///
/// TODO: It might be faster to generate args directly from
/// SILFunctionConventions without drilling through the result SILType. But one
/// way or annother, we need to map result types into this context.
unsigned
OpaqueStorageAllocation::insertIndirectResultParameters(unsigned argIdx,
                                                        SILType resultType) {
  auto &M = pass.F->getModule();

  // Expand tuples.
  if (auto tupleType = resultType.getAs<TupleType>()) {
    for (auto eltType : tupleType.getElementTypes()) {
      SILType eltTy = M.Types.getTypeLowering(eltType).getLoweredType();
      argIdx = insertIndirectResultParameters(argIdx, eltTy);
    }
    return argIdx;
  }

  // If the return type is address-only, emit the indirect return argument.
  if (!resultType.isAddressOnly(M))
    return argIdx;

  auto &ctx = pass.F->getModule().getASTContext();
  auto var = new (ctx)
      ParamDecl(/*IsLet*/ false, SourceLoc(), SourceLoc(),
                ctx.getIdentifier("$return_value"), SourceLoc(),
                ctx.getIdentifier("$return_value"),
                resultType.getSwiftRValueType(), pass.F->getDeclContext());

  pass.F->begin()->insertFunctionArgument(argIdx, resultType.getAddressType(),
                                          ValueOwnershipKind::Trivial, var);
  return argIdx + 1;
}

/// Utility to derive SILLocation.
/// TODO: This should be a common utility.
static SILLocation getLocForValue(SILValue value) {
  if (auto *instr = dyn_cast<SILInstruction>(value)) {
    return instr->getLoc();
  }
  if (auto *arg = dyn_cast<SILArgument>(value)) {
    if (arg->getDecl())
      return RegularLocation(const_cast<ValueDecl *>(arg->getDecl()));
  }
  // TODO: bbargs should probably use one of their operand locations.
  return value->getFunction()->getLocation();
}

/// Create a dealloc_stack instruction.
static DeallocStackInst *
createDeallocStackAfterBlock(SILBasicBlock *deallocBlock,
                             AllocStackInst *allocInstr) {
  SILBuilder deallocBuilder(deallocBlock->getTerminator());
  return deallocBuilder.createDeallocStack(allocInstr->getLoc(), allocInstr);
}

/// Allocate storage for a single opaque/resilient value.
void OpaqueStorageAllocation::allocateForValue(SILValue value,
                                               ValueStorage &storage) {
  assert(!isa<SILFunctionArgument>(value));
  // Tuples are in the value map so they can be deleted in the proper order. But
  // each tuple element already has its own storage.
  if (isa<TupleInst>(value))
    return;

  // Argument loads already have a storage address.
  if (storage.storageAddress) {
    assert(isa<SILFunctionArgument>(storage.storageAddress));
    return;
  }

  AllocStackInst *allocInstr =
      allocBuilder.createAllocStack(getLocForValue(value), value->getType());

  storage.storageAddress = allocInstr;

  // TODO: insert deallocation at reasonable points.
  auto blockI = pass.F->findReturnBB();
  if (blockI != pass.F->end())
    createDeallocStackAfterBlock(&*blockI, allocInstr);

  if (pass.F->findThrowBB() != pass.F->end())
    createDeallocStackAfterBlock(&*blockI, allocInstr);
}

/// Deallocate temporary call-site stack storage.
static void insertStackDeallocationAtCall(AllocStackInst *allocInst,
                                          SILInstruction *applyInst) {
  switch (applyInst->getKind()) {
  case ValueKind::ApplyInst: {
    SILBuilder deallocBuilder(applyInst);
    deallocBuilder.createDeallocStack(allocInst->getLoc(), allocInst);
    break;
  }
  case ValueKind::TryApplyInst:
    // TODO!!!: insert dealloc in the catch block.
    llvm_unreachable("not implemented for this instruction!");
  case ValueKind::PartialApplyInst:
    llvm_unreachable("partial apply cannot have indirect results.");
  default:
    llvm_unreachable("not implemented for this instruction!");
  }
}

/// Allocate storage for formally indirect arguments.
/// Update the operand to this address.
/// After this, the SIL argument types no longer match SIL function conventions.
void OpaqueStorageAllocation::allocateForOperand(Operand *operand) {
  SILInstruction *apply = operand->getUser();
  SILValue argValue = operand->get();

  SILBuilder callBuilder(apply);
  AllocStackInst *allocInstr =
      callBuilder.createAllocStack(apply->getLoc(), argValue->getType());

  // This is a store [init], but qualifications aren't allowed.
  callBuilder.createStore(apply->getLoc(), argValue, allocInstr,
                          StoreOwnershipQualifier::Unqualified);

  operand->set(allocInstr);

  insertStackDeallocationAtCall(allocInstr, apply);
}

/// Allocate storage for formally indirect results at the given call site.
/// Create a new call instruction with indirect SIL arguments.
void OpaqueStorageAllocation::allocateForResults(SILInstruction *origInst) {
  ApplySite apply(origInst);
  SILFunctionConventions loweredFnConv(
      apply.getSubstCalleeType(),
      SILModuleConventions::getLoweredAddressConventions());

  SILLocation loc = origInst->getLoc();
  SILBuilder callBuilder(origInst);

  SmallVector<SILValue, 8> args(loweredFnConv.getNumSILArguments());
  unsigned firstResultIdx = loweredFnConv.getSILArgIndexOfFirstIndirectResult();

  // Find the storage location of each indirect result and populate the SIL
  // argument vector.
  SmallVector<TupleExtractInst *, 4> concreteResults;
  if (origInst->getType().is<TupleType>()) {
    for (Operand *operand : origInst->getUses()) {
      if (auto *extract = dyn_cast<TupleExtractInst>(operand->getUser())) {
        unsigned argIdx = firstResultIdx + extract->getFieldNo();
        assert(argIdx < loweredFnConv.getSILArgIndexOfFirstParam());
        if (!extract->getType().isAddressOnly(pass.F->getModule())) {
          concreteResults.push_back(extract);
          continue;
        }
        auto addr = pass.valueStorageMap.getStorage(extract).storageAddress;
        assert(addr);
        args[argIdx] = addr;
      }
    }
  } else {
    auto addr = pass.valueStorageMap.getStorage(origInst).storageAddress;
    assert(addr);
    args[firstResultIdx] = addr;
  }
  // Allocate storage for any unused or concrete results. One for each missing
  // entry in the the SIL arguments list.
  unsigned argIdx = firstResultIdx;
  for (SILType resultTy : loweredFnConv.getIndirectSILResultTypes()) {
    if (!args[argIdx]) {
      AllocStackInst *allocInstr = callBuilder.createAllocStack(loc, resultTy);
      args[argIdx] = allocInstr;
      insertStackDeallocationAtCall(allocInstr, origInst);
    }
    ++argIdx;
  }
  // Append the existing call arguments to the SIL argument list. They were
  // already lowered to addresses by allocateForOperand.
  assert(argIdx == loweredFnConv.getSILArgIndexOfFirstParam());
  unsigned origArgIdx = apply.getSubstCalleeConv().getSILArgIndexOfFirstParam();
  for (unsigned endIdx = args.size(); argIdx < endIdx; ++argIdx, ++origArgIdx) {
    args[argIdx] = apply.getArgument(origArgIdx);
  }
  // Create a new apply with indirect result operands.
  SILInstruction *callInst;
  switch (origInst->getKind()) {
  case ValueKind::ApplyInst:
    callInst = callBuilder.createApply(
        loc, apply.getCallee(), apply.getSubstCalleeSILType(), apply.getType(),
        apply.getSubstitutions(), args,
        cast<ApplyInst>(origInst)->isNonThrowing());
    break;
  case ValueKind::TryApplyInst:
    // TODO: insert dealloc in the catch block.
    llvm_unreachable("not implemented for this instruction!");
  case ValueKind::PartialApplyInst:
  // Partial apply does not have formally indirect results.
  default:
    llvm_unreachable("not implemented for this instruction!");
  }
  if (pass.valueStorageMap.contains(origInst)) {
    pass.valueStorageMap.insertValue(callInst);
    pass.valueStorageMap.getStorage(callInst).storageAddress =
        pass.valueStorageMap.getStorage(origInst).storageAddress;
    // origInst remains in the map but has no users.
  }
  origInst->replaceAllUsesWith(callInst);
  pass.instsToDelete.insert(origInst);
  // Load an concrete args, and mark the extract for deletion.
  for (TupleExtractInst *extract : concreteResults) {
    unsigned argIdx = firstResultIdx + extract->getFieldNo();
    SILValue arg = args[argIdx];
    LoadInst *loadArg = callBuilder.createLoad(
        extract->getLoc(), arg, LoadOwnershipQualifier::Unqualified);
    extract->replaceAllUsesWith(loadArg);
    pass.instsToDelete.insert(extract);
  }
}

//===----------------------------------------------------------------------===//
// AddressOnlyRewriter - rewrite operations on address-only values.
//===----------------------------------------------------------------------===//
namespace {

class AddressOnlyRewriter : SILInstructionVisitor<AddressOnlyRewriter, void> {
  friend SILVisitor<AddressOnlyRewriter, void>;

  AddressLoweringState &pass;

  SILBuilder B;
  ValueStorage valueStorage;
  // Currently visited operand.
  Operand *currOper = nullptr;

public:
  explicit AddressOnlyRewriter(AddressLoweringState &pass)
      : pass(pass), B(*pass.F) {}

  void rewriteFunction() {
    for (auto &valueStorageI : pass.valueStorageMap) {
      valueStorage = valueStorageI.second;
      DEBUG(llvm::dbgs() << "VALUE   "; valueStorageI.first->dump());
      DEBUG(if (valueStorage.storageAddress) {
        llvm::dbgs() << "STORAGE ";
        valueStorage.storageAddress->dump();
      });
      for (Operand *currOper : valueStorageI.first->getUses()) {
        visit(currOper->getUser());
      }
    }
  }

protected:
  void visitValueBase(ValueBase *V) {
    DEBUG(V->dump());
    llvm_unreachable("Unimplemented?!");
  }

  void beforeVisit(ValueBase *V) {
    DEBUG(llvm::dbgs() << "REWRITE "; V->dump());
    auto *I = cast<SILInstruction>(V);
    B.setInsertionPoint(I);
    B.setCurrentDebugScope(I->getDebugScope());
  }

  void visitApplyInst(ApplyInst *applyInst) {
    DEBUG(llvm::dbgs() << "CALL "; applyInst->dump();
          llvm::dbgs() << "OPERAND "; currOper->get()->dump());
    llvm_unreachable("Unhandled call result.");
  }

  void visitCopyValueInst(CopyValueInst *copyInst) {
    SILValue srcAddr =
        pass.valueStorageMap.getStorage(copyInst->getOperand()).storageAddress;
    SILValue destAddr =
        pass.valueStorageMap.getStorage(copyInst).storageAddress;
    B.createCopyAddr(copyInst->getLoc(), srcAddr, destAddr, IsNotTake,
                     IsInitialization);
  }

  void visitTupleInst(TupleInst *tupleInst) {
    // Tuple elements have their own storage. Tuple instructions are dead.
    assert(!pass.valueStorageMap.getStorage(tupleInst).storageAddress);
  }

  void visitTupleExtractInst(TupleExtractInst *TEI) {
    // Tuple element instructions don't require rewrite. They are dead.
    llvm_unreachable("Untested.");
    return;
  }

  void visitReturnInst(ReturnInst *returnInst) {
    SILFunctionConventions loweredFnConv(
        pass.F->getLoweredFunctionType(),
        SILModuleConventions::getLoweredAddressConventions());

    unsigned resultArgIdx = loweredFnConv.getSILArgIndexOfFirstIndirectResult();
    auto copyResult = [&](SILValue result) {
      SILArgument *resultArg = B.getFunction().getArgument(resultArgIdx);
      SILValue resultAddr =
          pass.valueStorageMap.getStorage(result).storageAddress;
      B.createCopyAddr(returnInst->getLoc(), resultAddr, resultArg, IsTake,
                       IsInitialization);
    };
    unsigned numIndirectResults = loweredFnConv.getNumIndirectSILResults();
    if (numIndirectResults == 1) {
      copyResult(returnInst->getOperand());
    } else {
      for (unsigned endIdx = resultArgIdx + numIndirectResults;
           resultArgIdx < endIdx; ++resultArgIdx) {
        auto *tupleInst = cast<TupleInst>(returnInst->getOperand());
        copyResult(tupleInst->getElement(resultArgIdx));
      }
    }
    SILType emptyTy = B.getModule().Types.getLoweredType(
        TupleType::getEmpty(B.getModule().getASTContext()));
    auto *tupleInst = B.createTuple(returnInst->getLoc(), emptyTy, {});
    returnInst->setOperand(tupleInst);
  }
};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// AddressLowering: Top Level Function Transform.
//===----------------------------------------------------------------------===//

namespace {
class AddressLowering : public SILModuleTransform {
  /// The entry point to this function transformation.
  void run() override;

  StringRef getName() override { return "Address Lowering"; }

  void runOnFunction(SILFunction *F);
};
} // end anonymous namespace

void AddressLowering::runOnFunction(SILFunction *F) {
  AddressLoweringState pass(F);

  // Rewrite function args and insert alloc_stack/dealloc_stack.
  OpaqueStorageAllocation allocator(pass);
  allocator.allocateOpaqueStorage();

  // Rewrite instructions with address-only operands or results.
  AddressOnlyRewriter(pass).rewriteFunction();

  invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);

  // Instructions that were explicitly marked dead should already have no
  // users.
  //
  // Add the rest of the instructions to the dead list in post order.
  // FIXME: make sure we cleaned up address-only BB arguments.
  for (auto &valueStorageI : reversed(pass.valueStorageMap)) {
    auto *deadInst = dyn_cast<SILInstruction>(valueStorageI.first);
    if (!deadInst)
      continue;

    DEBUG(llvm::dbgs() << "DEAD "; deadInst->dump());
    for (Operand *operand : deadInst->getUses())
      assert(pass.instsToDelete.count(operand->getUser()));

    pass.instsToDelete.insert(deadInst);
  }
  pass.valueStorageMap.clear();

  // Delete instructions in postorder
  recursivelyDeleteTriviallyDeadInstructions(pass.instsToDelete.takeVector(),
                                             true);
}

/// The entry point to this function transformation.
void AddressLowering::run() {
  if (getModule()->getASTContext().LangOpts.EnableSILOpaqueValues) {
    for (auto &F : *getModule())
      runOnFunction(&F);
  }
  // Set the SIL state before the PassManager has a chance to run
  // verification.
  getModule()->setStage(SILStage::Lowered);
}

SILTransform *swift::createAddressLowering() { return new AddressLowering(); }
