//===--- AddressLowering.cpp - Lower SIL address-only types. --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
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
#include "swift/SIL/DebugUtils.h"
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

  ValueStorage &getStorage(SILValue value) {
    auto hashIter = valueHashMap.find(value);
    assert(hashIter != valueHashMap.end() && "Missing SILValue");
    return valueVector[hashIter->second].second;
  }

  ValueStorage &insertValue(SILValue value) {
    auto hashResult =
        valueHashMap.insert(std::make_pair(value, valueVector.size()));
    assert(hashResult.second && "SILValue already mapped");

    valueVector.emplace_back(value, ValueStorage());

    return valueVector.back().second;
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
  // All function-exiting terminators (return or throw instructions).
  SmallVector<TermInst *, 8> returnInsts;
  // Delete these instructions after performing transformations.
  // They must not have any remaining users.
  SmallSetVector<SILInstruction *, 16> instsToDelete;

  AddressLoweringState(SILFunction *F) : F(F) {}

  void markDeadInst(SILInstruction *inst) {
#ifndef NDEBUG
    for (Operand *use : inst->getUses())
      assert(instsToDelete.count(use->getUser()));
#endif
    instsToDelete.insert(inst);
  }
};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// OpaqueValueVisitor: Map OpaqueValues to ValueStorage.
//===----------------------------------------------------------------------===//

namespace {
/// Collect all opaque/resilient values, inserting them in `valueStorageMap` in
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

/// Top-level entry: Populate `valueStorageMap`, `indirectResults`, and
/// `indirectOperands`.
///
/// Find all Opaque/Resilient SILValues and add them
/// to valueStorageMap in RPO.
void OpaqueValueVisitor::mapValueStorage() {
  for (auto *BB : postorderInfo.getReversePostOrder()) {
    if (BB->getTerminator()->isFunctionExiting())
      pass.returnInsts.push_back(BB->getTerminator());

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

/// Populate `indirectResults` and `indirectOperands`.
///
/// If this call has indirect formal results, add it to `indirectResults`.
/// 
/// Add each call operand with a formally indirect convention to
/// `indirectOperands`.
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

/// If `value` is address-only add it to the `valueStorageMap`.
/// 
/// TODO: Set ValueStorage.projectionFrom whenever there is a single
/// isComposingOperand() use and the aggregate's storage can be hoisted to a
/// dominating point.
void OpaqueValueVisitor::visitValue(SILValue value) {
  if (value->getType().isObject()
      && value->getType().isAddressOnly(pass.F->getModule())) {
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

public:
  explicit OpaqueStorageAllocation(AddressLoweringState &pass) : pass(pass) {}

  void allocateOpaqueStorage();

protected:
  void replaceFunctionArgs();
  unsigned insertIndirectResultParameters(unsigned argIdx, SILType resultType);
  void allocateForValue(SILValue value, ValueStorage &storage);
  void allocateForOperand(Operand *operand);
  void canonicalizeResults(
    ApplyInst *applyInst,
    SmallVectorImpl<SILInstruction*> &directResultValues,
    ArrayRef<Operand*> nonCanonicalUses);
  void canonicalizeResults(ApplyInst *applyInst);
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

  // Create an AllocStack for every opaque value defined in the function.
  for (auto &valueStorageI : pass.valueStorageMap)
    allocateForValue(valueStorageI.first, valueStorageI.second);

  // Create an AllocStack for every formally indirect argument.
  for (Operand *operand : pass.indirectOperands)
    allocateForOperand(operand);
  pass.indirectOperands.clear();

  // Create an AllocStack for every formally indirect result.
  // This rewrites and potentially erases the original call.
  for (SILInstruction *callInst : pass.indirectResults)
    allocateForResults(callInst);
  pass.indirectResults.clear();
}

/// Replace each value-typed argument to the current function with an
/// address-typed argument by inserting a temporary load instruction.
void OpaqueStorageAllocation::replaceFunctionArgs() {
  // Insert temporary argument loads at the top of the function.
  SILBuilder argBuilder(pass.F->getEntryBlock()->begin());

  auto fnConv = pass.F->getConventions();
  unsigned argIdx = fnConv.getSILArgIndexOfFirstParam();
  for (SILParameterInfo param :
       pass.F->getLoweredFunctionType()->getParameters()) {

    if (param.isFormalIndirect() && !fnConv.isSILIndirect(param)) {
      SILArgument *arg = pass.F->getArgument(argIdx);
      SILType addrType = arg->getType().getAddressType();

      LoadInst *loadArg = argBuilder.createLoad(
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
/// Note: This is effectively moved from the old SILGen
/// emitIndirectResultParameters.
///
/// TODO: It might be faster to generate args directly from
/// SILFunctionConventions without drilling through the result SILType. But one
/// way or another, we need to map result types into this context.
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
/// 
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

/// Allocate storage for a single opaque/resilient value.
void OpaqueStorageAllocation::allocateForValue(SILValue value,
                                               ValueStorage &storage) {
  assert(!isa<SILFunctionArgument>(value));
  // Tuples are in the value map so they can be deleted in the proper order. But
  // each tuple element already has its own storage.
  if (isa<TupleInst>(value))
    return;

  // Don't bother allocating storage for result tuples. Each element has its own
  // storage.
  if (isa<FullApplySite>(value))
    return;

  // Argument loads already have a storage address.
  if (storage.storageAddress) {
    assert(isa<SILFunctionArgument>(storage.storageAddress));
    return;
  }

  SILBuilder allocBuilder(pass.F->begin()->begin());
  AllocStackInst *allocInstr =
      allocBuilder.createAllocStack(getLocForValue(value), value->getType());

  storage.storageAddress = allocInstr;

  // Insert stack deallocations.
  for (TermInst *termInst : pass.returnInsts) {
    SILBuilder deallocBuilder(termInst);
    deallocBuilder.createDeallocStack(allocInstr->getLoc(), allocInstr);
  }
}

/// Deallocate temporary call-site stack storage.
///
/// `argLoad` is non-null for @out args that are loaded.
static void insertStackDeallocationAtCall(AllocStackInst *allocInst,
                                          SILInstruction *applyInst,
                                          SILInstruction *argLoad) {
  SILInstruction *lastUse = argLoad ? argLoad : applyInst;

  switch (applyInst->getKind()) {
  case ValueKind::ApplyInst: {
    SILBuilder deallocBuilder(&*std::next(lastUse->getIterator()));
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
///
/// Note: The temporary argument storage does not own its value. If the argument
/// is owner, the stored value should already have been copied.
void OpaqueStorageAllocation::allocateForOperand(Operand *operand) {
  SILInstruction *apply = operand->getUser();
  SILValue argValue = operand->get();

  SILBuilder callBuilder(apply);
  AllocStackInst *allocInstr =
      callBuilder.createAllocStack(apply->getLoc(), argValue->getType());

  callBuilder.createStore(apply->getLoc(), argValue, allocInstr,
                          StoreOwnershipQualifier::Unqualified);

  operand->set(allocInstr);

  insertStackDeallocationAtCall(allocInstr, apply, /*argLoad=*/nullptr);
}

// Canonicalize call result uses. Treat each result of a multi-result call as
// independent values. Currently, SILGen may generate tuple_extract for each
// result but generate a single destroy_value for the entire tuple of
// results. This makes it impossible to reason about each call result as an
// independent value according to the callee's function type.
//
// directResultValues has an entry for each tuple extract corresponding to
// that result if one exists. This function will add an entry to
// directResultValues whenever it needs to materialize a TupleExtractInst.
void OpaqueStorageAllocation::canonicalizeResults(
  ApplyInst *applyInst,
  SmallVectorImpl<SILInstruction*> &directResultValues,
  ArrayRef<Operand*> nonCanonicalUses) {

  for (Operand *operand : nonCanonicalUses) {
    auto *destroyInst = dyn_cast<DestroyValueInst>(operand->getUser());
    if (!destroyInst)
      llvm::report_fatal_error("Simultaneous use of multiple call results.");

    for (unsigned resultIdx = 0, endIdx = directResultValues.size();
         resultIdx < endIdx; ++resultIdx) {
      SILInstruction *result = directResultValues[resultIdx];
      if (!result) {
        SILBuilder resultBuilder(std::next(SILBasicBlock::iterator(applyInst)));
        result = resultBuilder.createTupleExtract(applyInst->getLoc(),
                                                  applyInst, resultIdx);
        directResultValues[resultIdx] = result;
      }
      SILBuilder B(destroyInst);
      auto &TL = pass.F->getModule().getTypeLowering(result->getType());
      TL.emitDestroyValue(B, destroyInst->getLoc(), result);
    }
    destroyInst->eraseFromParent();
  }
}

/// Allocate storage for formally indirect results at the given call site.
/// Create a new call instruction with indirect SIL arguments.
void OpaqueStorageAllocation::allocateForResults(SILInstruction *origCallInst) {
  ApplySite apply(origCallInst);

  SILFunctionConventions origFnConv = apply.getSubstCalleeConv();

  // Gather the original direct return values.
  // Canonicalize results so no user uses more than one result.
  SmallVector<SILInstruction *, 8> origDirectResultValues(
    origFnConv.getNumDirectSILResults());
  SmallVector<Operand *, 4> nonCanonicalUses;
  if (origCallInst->getType().is<TupleType>()) {
    for (Operand *operand : origCallInst->getUses()) {
      if (auto *extract = dyn_cast<TupleExtractInst>(operand->getUser()))
        origDirectResultValues[extract->getFieldNo()] = extract;
      else
        nonCanonicalUses.push_back(operand);
    }
    if (!nonCanonicalUses.empty()) {
      auto *applyInst = cast<ApplyInst>(origCallInst);
      canonicalizeResults(applyInst, origDirectResultValues, nonCanonicalUses);
    }
  } else {
    // A call with no indirect results should not have been added to
    // pass.indirectResults.
    assert(origDirectResultValues.size() == 1);
    if (!origCallInst->use_empty()) {
      origDirectResultValues[0] = origCallInst;
      // Tuple extract values were already mapped. The call itself was not.
      pass.valueStorageMap.insertValue(origCallInst);
    }
  }

  // Prepare to emit a new call instruction.
  SILLocation loc = origCallInst->getLoc();
  SILBuilder callBuilder(origCallInst);

  // Lambda to allocate and map storage for an indirect result.
  // Note: origDirectResultValues contains null for unused results.
  auto mapIndirectArg = [&](SILInstruction *origDirectResultVal,
                            SILType argTy) {
    if (origDirectResultVal
        && pass.valueStorageMap.contains(origDirectResultVal)) {
      // Pass the local storage address as the indirect result address.
      return
        pass.valueStorageMap.getStorage(origDirectResultVal).storageAddress;
    }
    // Allocate temporary call-site storage for an unused or loadable result.
    auto *allocInst =
    callBuilder.createAllocStack(loc, argTy);
    LoadInst *loadInst = nullptr;
    if (origDirectResultVal) {
      // TODO: Find the try_apply's result block.
      // Build results outside-in to next stack allocations.
      SILBuilder resultBuilder(
        std::next(SILBasicBlock::iterator(origCallInst)));
      // This is a formally indirect argument, but is loadable.
      loadInst = resultBuilder.createLoad(loc, allocInst,
                                          LoadOwnershipQualifier::Unqualified);
      origDirectResultVal->replaceAllUsesWith(loadInst);
      pass.markDeadInst(origDirectResultVal);
    }
    insertStackDeallocationAtCall(allocInst, origCallInst, loadInst);
    return SILValue(allocInst);
  };

  // The new call instruction's SIL calling convention.
  SILFunctionConventions loweredFnConv(
      apply.getSubstCalleeType(),
      SILModuleConventions::getLoweredAddressConventions());

  // The new call instruction's SIL argument list.
  SmallVector<SILValue, 8> newCallArgs(loweredFnConv.getNumSILArguments());

  // Map the original result indices to new result indices.
  SmallVector<unsigned, 8> newDirectResultIndices(
    origFnConv.getNumDirectSILResults());
  // Indices used to populate newDirectResultIndices.
  unsigned oldDirectResultIdx = 0, newDirectResultIdx = 0;

  // The index of the next indirect result argument.
  unsigned newResultArgIdx =
    loweredFnConv.getSILArgIndexOfFirstIndirectResult();

  // Visit each result. Redirect results that are now indirect.
  // Result that remain direct will be redirected later.
  // Populate newCallArgs and newDirectResultIndices.
  for_each(
    apply.getSubstCalleeType()->getResults(),
    origDirectResultValues, 
    [&](SILResultInfo resultInfo, SILInstruction *origDirectResultVal) {
      // Assume that all original results are direct in SIL.
      assert(!origFnConv.isSILIndirect(resultInfo));

      if (loweredFnConv.isSILIndirect(resultInfo)) {
        newCallArgs[newResultArgIdx++] =
          mapIndirectArg(origDirectResultVal,
                         loweredFnConv.getSILType(resultInfo));;
        newDirectResultIndices[oldDirectResultIdx++] = newDirectResultIdx;
      } else {
        newDirectResultIndices[oldDirectResultIdx++] = newDirectResultIdx++;
      }
      // replaceAllUses will be called later to handle direct results that
      // remain direct results of the new call instruction.
    });

  // Append the existing call arguments to the SIL argument list. They were
  // already lowered to addresses by allocateForOperand.
  assert(newResultArgIdx == loweredFnConv.getSILArgIndexOfFirstParam());
  unsigned origArgIdx = apply.getSubstCalleeConv().getSILArgIndexOfFirstParam();
  for (unsigned endIdx = newCallArgs.size(); newResultArgIdx < endIdx;
       ++newResultArgIdx, ++origArgIdx) {
    newCallArgs[newResultArgIdx] = apply.getArgument(origArgIdx);
  }

  // Create a new apply with indirect result operands.
  SILInstruction *newCallInst;
  switch (origCallInst->getKind()) {
  case ValueKind::ApplyInst:
    newCallInst = callBuilder.createApply(
        loc, apply.getCallee(), apply.getSubstCalleeSILType(),
        loweredFnConv.getSILResultType(), apply.getSubstitutions(), newCallArgs,
        cast<ApplyInst>(origCallInst)->isNonThrowing());
    break;
  case ValueKind::TryApplyInst:
    // TODO: insert dealloc in the catch block.
    llvm_unreachable("not implemented for this instruction!");
  case ValueKind::PartialApplyInst:
  // Partial apply does not have formally indirect results.
  default:
    llvm_unreachable("not implemented for this instruction!");
  }

  // Replace all unmapped uses of the original call with uses of the new call.
  // 
  // TODO: handle bbargs from try_apply.
  SILBuilder resultBuilder(
    std::next(SILBasicBlock::iterator(origCallInst)));
  SmallVector<Operand*, 8> origUses(origCallInst->getUses());
  for (Operand *operand : origUses) {
    auto *extractInst = dyn_cast<TupleExtractInst>(operand->getUser());
    if (!extractInst) {
      assert(origFnConv.getNumDirectSILResults() == 1);
      assert(pass.valueStorageMap.contains(origCallInst));
      continue;
    }
    unsigned origResultIdx = extractInst->getFieldNo();
    auto resultInfo = origFnConv.getResults()[origResultIdx];

    if (extractInst->getType().isAddressOnly(pass.F->getModule())) {
      assert(loweredFnConv.isSILIndirect(resultInfo));
      assert(pass.valueStorageMap.contains(extractInst));
      continue;
    }
    if (loweredFnConv.isSILIndirect(resultInfo)) {
      // This loadable indirect use should already be
      // redirected to a load from the argument storage and marked dead.
      assert(extractInst->use_empty());
      continue;
    }
    // Either the new call instruction has only a single direct result, or we
    // map the original tuple field to the new tuple field.
    SILInstruction *newValue = newCallInst;
    if (loweredFnConv.getNumDirectSILResults() > 1) {
      assert(newCallInst->getType().is<TupleType>());
      newValue = resultBuilder.createTupleExtract(
        extractInst->getLoc(), newCallInst,
        newDirectResultIndices[origResultIdx]);
    }
    extractInst->replaceAllUsesWith(newValue);
    extractInst->eraseFromParent();
  }
  if (!pass.valueStorageMap.contains(origCallInst))
    pass.markDeadInst(origCallInst);
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
      SILValue valueDef = valueStorageI.first;
      valueStorage = valueStorageI.second;
      DEBUG(llvm::dbgs() << "VALUE   "; valueDef->dump());
      DEBUG(if (valueStorage.storageAddress) {
        llvm::dbgs() << "  STORAGE ";
        valueStorage.storageAddress->dump();
      });
      SmallVector<Operand*, 8> uses(valueDef->getUses());
      for (Operand *oper : uses) {
        currOper = oper;
        visit(currOper->getUser());
      }
      currOper = nullptr;
      if (auto *defInst = dyn_cast<SILInstruction>(valueDef))
        visit(defInst);
    }
  }

protected:
  void visitValueBase(ValueBase *V) {
    DEBUG(V->dump());
    llvm_unreachable("Unimplemented?!");
  }

  void beforeVisit(ValueBase *V) {
    DEBUG(llvm::dbgs() << "  REWRITE "; V->dump());
    auto *I = cast<SILInstruction>(V);
    B.setInsertionPoint(I);
    B.setCurrentDebugScope(I->getDebugScope());
  }

  void visitApplyInst(ApplyInst *applyInst) {
    if (currOper) {
      DEBUG(llvm::dbgs() << "  CALL "; applyInst->dump();
            llvm::dbgs() << "  OPERAND "; currOper->get()->dump());
      llvm_unreachable("Unhandled call result.");
    }
  }

  void visitCopyValueInst(CopyValueInst *copyInst) {
    if (!currOper)
      return;
    
    SILValue srcAddr =
        pass.valueStorageMap.getStorage(copyInst->getOperand()).storageAddress;
    SILValue destAddr =
        pass.valueStorageMap.getStorage(copyInst).storageAddress;
    B.createCopyAddr(copyInst->getLoc(), srcAddr, destAddr, IsNotTake,
                     IsInitialization);
  }
  
  void visitDebugValueInst(DebugValueInst *debugInst) {
    SILValue addr =
      pass.valueStorageMap.getStorage((debugInst->getOperand())).storageAddress;
    B.createDebugValueAddr(debugInst->getLoc(), addr);
    pass.markDeadInst(debugInst);
  }
  
  void visitDestroyValueInst(DestroyValueInst *destroyInst) {
    assert(currOper->get() == destroyInst->getOperand());
    SILValue src = currOper->get();
    SILValue addr = pass.valueStorageMap.getStorage(src).storageAddress;
    B.createDestroyAddr(destroyInst->getLoc(), addr);
    pass.markDeadInst(destroyInst);
  }

  void visitLoadInst(LoadInst *loadInst) {
    assert(!currOper);
    // Bitwise copy the value. Two locations now share ownership. This is
    // modeled as a take-init.
    SILValue addr = pass.valueStorageMap.getStorage(loadInst).storageAddress;
    if (addr != loadInst->getOperand()) {
      B.createCopyAddr(loadInst->getLoc(), loadInst->getOperand(), addr,
                       IsTake, IsInitialization);
    }
  }

  void visitReturnInst(ReturnInst *returnInst) {
    assert(currOper->get() == returnInst->getOperand());

    auto insertPt = SILBasicBlock::iterator(returnInst);
    auto bbStart = returnInst->getParent()->begin();
    while (insertPt != bbStart) {
      --insertPt;
      if (!isa<DeallocStackInst>(*insertPt))
        break;
    }
    B.setInsertionPoint(insertPt);

    // Gather direct function results.
    unsigned numOrigDirectResults =
      pass.F->getConventions().getNumDirectSILResults();
    SmallVector<SILValue, 8> origDirectResultValues;
    if (numOrigDirectResults == 1)
      origDirectResultValues.push_back(currOper->get());
    else {
      auto *tupleInst = cast<TupleInst>(currOper->get());
      origDirectResultValues.append(tupleInst->getElements().begin(),
                                    tupleInst->getElements().end());
      assert(origDirectResultValues.size() == numOrigDirectResults);
    }

    SILFunctionConventions origFnConv(pass.F->getConventions());
    SILFunctionConventions loweredFnConv(
        pass.F->getLoweredFunctionType(),
        SILModuleConventions::getLoweredAddressConventions());

    // Convert each result.
    SmallVector<SILValue, 8> newDirectResults;
    unsigned newResultArgIdx =
      loweredFnConv.getSILArgIndexOfFirstIndirectResult();

    for_each(
      pass.F->getLoweredFunctionType()->getResults(),
      origDirectResultValues, 
      [&](SILResultInfo resultInfo, SILValue origDirectResultVal) 
      {
        // Assume that all original results are direct in SIL.
        assert(!origFnConv.isSILIndirect(resultInfo));
        if (loweredFnConv.isSILIndirect(resultInfo)) {
          assert(newResultArgIdx < loweredFnConv.getSILArgIndexOfFirstParam());
          SILArgument *resultArg = B.getFunction().getArgument(newResultArgIdx);
          if (pass.valueStorageMap.contains(origDirectResultVal)) {
            // Copy the result from local storage into the result argument.
            SILValue resultAddr =
              pass.valueStorageMap.getStorage(origDirectResultVal)
              .storageAddress;

            B.createCopyAddr(returnInst->getLoc(), resultAddr, resultArg,
                             IsTake, IsInitialization);
            ++newResultArgIdx;
          }
          else {
            // Sore the result into the result argument.
            B.createStore(returnInst->getLoc(), origDirectResultVal,
                          resultArg, StoreOwnershipQualifier::Init);
            llvm_unreachable("untested."); //!!!
          }
        } else {
          // Record the direct result for populating the result tuple.
          newDirectResults.push_back(origDirectResultVal);
        }
      });
    assert(newDirectResults.size() == loweredFnConv.getNumDirectSILResults());
    SILValue newReturnVal;
    if (newDirectResults.empty()) {
      SILType emptyTy = B.getModule().Types.getLoweredType(
        TupleType::getEmpty(B.getModule().getASTContext()));
      newReturnVal = B.createTuple(returnInst->getLoc(), emptyTy, {});
    } else if (newDirectResults.size() == 1) {
      newReturnVal = newDirectResults[0];
    } else {
      newReturnVal = B.createTuple(returnInst->getLoc(),
                                   loweredFnConv.getSILResultType(),
                                   newDirectResults);
    }
    returnInst->setOperand(newReturnVal);
  }

  void visitStoreInst(StoreInst *storeInst) {
    assert(currOper->get() == storeInst->getSrc());
    SILValue srcAddr =
      pass.valueStorageMap.getStorage(currOper->get()).storageAddress;
    assert(storeInst->getOwnershipQualifier() ==
           StoreOwnershipQualifier::Unqualified);

    // Bitwise copy the value. Two locations now share ownership. This is
    // modeled as a take-init.
    B.createCopyAddr(storeInst->getLoc(), srcAddr, storeInst->getDest(),
                     IsTake, IsInitialization);
    pass.markDeadInst(storeInst);
  }

  void visitTupleInst(TupleInst *tupleInst) {
    // Tuple elements have their own storage. Tuple instructions are dead.
    assert(!pass.valueStorageMap.getStorage(tupleInst).storageAddress);
  }

  void visitTupleExtractInst(TupleExtractInst *extractInst) {
    // TupleExtract merely names a storage location. It doesn't need to be
    // rewritten.
    assert(extractInst->use_empty()
           || pass.valueStorageMap.getStorage(extractInst).storageAddress);
  }
};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// AddressLowering: Top-Level Function Transform.
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

  DEBUG(llvm::dbgs() << "\nREWRITING: " << F->getName(); F->dump());

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
