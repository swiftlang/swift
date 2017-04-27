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
// In the following text, items marked "[REUSE]" only apply to the proposed
// storage reuse optimization, which is not currently implemented.
//
// ## State
//
// A `valueStorageMap` maps each opaque SIL value to its storage
// information containing:
//
// - An ordinal representing the position of this instruction.
//
// - [REUSE] The identifier of the storage object. An optimized storage object
//   may have multiple disjoint lifetimes. A storage object may also have
//   subobjects. Each subobject has its own live range. When considering
//   liveness of the subobject, one must also consider liveness of the
//   parent object.
//
// - If this is a subobject projection, refer back to the value whose
//  storage object will be the parent that this storage address is a
//  projection of.
//
// - The storage address for this subobject.
//
// ## Step #1: Map opaque values
//
// Populate `valueStorageMap` in forward order (RPO), giving each opaque value
// an ordinal position.
//
// [REUSE] Assign a storage identifier to each opaque value. Optionally optimize
// storage by assigning multiple values the same identifier.
//
// ## Step #2: Allocate storage
//
// In reverse order (PO), allocate the parent storage object for each opaque
// value.
//
// [REUSE] If storage has already been allocated for the current live range,
// then simply reuse it.
//
// If the value's use composes a parent object from this value, and use's
// storage can be projected from, then mark the value's storage as a projection
// from the use value. [REUSE] Also inherit the use's storage identifier, and
// add an interval to the live range with the current projection path.
//
// A use can be projected from if its allocation is available at (dominates)
// this value and using the same storage over the interval from this value to
// the use does not overlap with the existing live range.
//
// Checking interference requries checking all operands that have been marked as
// projections. In the case of block arguments, it meens checking the terminator
// operands of all predecessor blocks.
//
// [REUSE] Rather than checking all value operands, each live range will contain
// a set of intervals. Each interval will be associated with a projection path.
//
// Opaque value's that are the root of all projection paths now have their
// `storageAddress` assigned to an `alloc_stack` or argument. Opaque value's
// that are projections do not yet have a `storageAddress`.
//
// ## Step #3. Rewrite opaque values
//
// In forward order (RPO), rewrite each opaque value definition, and all its
// uses. This generally involves creating a new `_addr` variant of the
// instruction and obtaining the storage address from the `valueStorageMap`.
//
// If this value's storage is a projection of the value defined by its composing
// use, then first generate instructions to materialize the projection. This is
// a recursive process starting with the root of the projection path.
//
// A projection path will be materialized once, for the leaf subobject. When
// this happens, the `storageAddress` will be assigned for any intermediate
// projection paths. When those values are rewritten, their `storageAddress`
// will already be available.
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
using llvm::PointerIntPair;

llvm::cl::opt<bool>
    OptimizeOpaqueAddressLowering("optimize-opaque-address-lowering",
                                  llvm::cl::init(false));

// Visit all call results.
// Stop when the visitor returns `false`.
void visitCallResults(ApplySite apply,
                      std::function<bool(SILInstruction *)> visitor) {
  if (apply.getType().is<TupleType>()) {
    for (auto *operand : apply.getInstruction()->getUses()) {
      if (!visitor(operand->getUser()))
        break;
    }
  } else
    visitor(apply.getInstruction());
}

//===----------------------------------------------------------------------===//
// ValueStorageMap: Map Opaque/Resilient SILValues to abstract storage units.
//===----------------------------------------------------------------------===//

namespace {
struct ValueStorage {
  enum { IsProjectionMask = 0x1, IsRewrittenMask = 0x2 };
  PointerIntPair<Operand *, 2, unsigned> projectionAndFlags;

  /// The final address of this storage unit after rewriting the SIL.
  /// For values linked to their own storage, this is set during storage
  /// allocation. For projections, it is only set after instruction rewriting.
  SILValue storageAddress;

  bool isProjection() const {
    return projectionAndFlags.getInt() & IsProjectionMask;
  }
  /// Return the operand the composes an aggregate from this value.
  Operand *getComposedOperand() const {
    assert(isProjection());
    return projectionAndFlags.getPointer();
  }
  void setComposedOperand(Operand *oper) {
    projectionAndFlags.setPointer(oper);
    projectionAndFlags.setInt(projectionAndFlags.getInt() | IsProjectionMask);
  }

  bool isRewritten() const {
    if (projectionAndFlags.getInt() & IsRewrittenMask) {
      assert(storageAddress);
      return true;
    }
    return false;
  }
  void markRewritten() {
    projectionAndFlags.setInt(projectionAndFlags.getInt() | IsRewrittenMask);
  }
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

  unsigned getOrdinal(SILValue value) {
    auto hashIter = valueHashMap.find(value);
    assert(hashIter != valueHashMap.end() && "Missing SILValue");
    return hashIter->second;
  }

  ValueStorage &getStorage(SILValue value) {
    return valueVector[getOrdinal(value)].second;
  }

  // This must be called in RPO order.
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
  SILFunctionConventions loweredFnConv;

  // Dominators remain valid throughout this pass.
  DominanceInfo *domInfo;

  // All opaque values and associated storage.
  ValueStorageMap valueStorageMap;
  // All call sites with formally indirect SILArgument or SILResult conventions.
  // Calls are removed from the set when rewritten.
  SmallSetVector<SILInstruction *, 16> indirectApplies;
  // All function-exiting terminators (return or throw instructions).
  SmallVector<TermInst *, 8> returnInsts;
  // Delete these instructions after performing transformations.
  // They must not have any remaining users.
  SmallSetVector<SILInstruction *, 16> instsToDelete;

  AddressLoweringState(SILFunction *F, DominanceInfo *domInfo)
      : F(F),
        loweredFnConv(F->getLoweredFunctionType(),
                      SILModuleConventions::getLoweredAddressConventions()),
        domInfo(domInfo) {}

  bool isDead(SILInstruction *inst) const { return instsToDelete.count(inst); }

  void markDead(SILInstruction *inst) {
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

/// Populate `indirectApplies` and insert this apply in `valueStorageMap` if
/// the call's non-tuple result is returned indirectly.
void OpaqueValueVisitor::visitApply(ApplySite applySite) {
  auto calleeConv = applySite.getSubstCalleeConv();
  unsigned calleeArgIdx = applySite.getCalleeArgIndexOfFirstAppliedArg();
  for (Operand &operand : applySite.getArgumentOperands()) {
    if (operand.get()->getType().isObject()) {
      auto argConv = calleeConv.getSILArgumentConvention(calleeArgIdx);
      if (argConv.isIndirectConvention()) {
        pass.indirectApplies.insert(applySite.getInstruction());
      }
    }
    ++calleeArgIdx;
  }

  if (applySite.getSubstCalleeType()->hasIndirectFormalResults()) {
    pass.indirectApplies.insert(applySite.getInstruction());
    if (!applySite.getType().is<TupleType>())
      pass.valueStorageMap.insertValue(applySite.getInstruction());

    return;
  }
}

/// If `value` is address-only add it to the `valueStorageMap`.
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
  void convertIndirectFunctionArgs();
  unsigned insertIndirectReturnArgs();
  bool canProjectFrom(SILInstruction *innerVal, SILInstruction *composingUse);
  void allocateForValue(SILValue value, ValueStorage &storage);
};
} // end anonymous namespace

/// Top-level entry point: allocate storage for all opaque/resilient values.
void OpaqueStorageAllocation::allocateOpaqueStorage() {
  /* TODO: I think we need a GenericContextScope for mapTypeIntoContext, but all
     tests are currently passing without it.
  auto canFnType = pass.F->getLoweredFunctionType();

  // Setup a generic context for argument and result types.
  swift::Lowering::GenericContextScope scope(pass.F->getModule().Types,
                                             canFnType->getGenericSignature());
  */
  // Fixup this function's argument types with temporary loads.
  convertIndirectFunctionArgs();

  // Create a new function argument for each indirect result.
  insertIndirectReturnArgs();

  // Populate valueStorageMap.
  OpaqueValueVisitor(pass).mapValueStorage();

  // Create an AllocStack for every opaque value defined in the function.  Visit
  // values in post-order to create storage for aggregates before subobjects.
  for (auto &valueStorageI : reversed(pass.valueStorageMap))
    allocateForValue(valueStorageI.first, valueStorageI.second);
}

/// Replace each value-typed argument to the current function with an
/// address-typed argument by inserting a temporary load instruction.
void OpaqueStorageAllocation::convertIndirectFunctionArgs() {
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

/// Insert function arguments for any @out result type. Return the number of
/// indirect result arguments added.
unsigned OpaqueStorageAllocation::insertIndirectReturnArgs() {
  auto &ctx = pass.F->getModule().getASTContext();
  unsigned argIdx = 0;
  for (auto resultTy : pass.loweredFnConv.getIndirectSILResultTypes()) {
    auto bodyResultTy = pass.F->mapTypeIntoContext(resultTy);
    auto var = new (ctx)
        ParamDecl(/*IsLet*/ false, SourceLoc(), SourceLoc(),
                  ctx.getIdentifier("$return_value"), SourceLoc(),
                  ctx.getIdentifier("$return_value"),
                  bodyResultTy.getSwiftRValueType(), pass.F->getDeclContext());

    pass.F->begin()->insertFunctionArgument(argIdx,
                                            bodyResultTy.getAddressType(),
                                            ValueOwnershipKind::Trivial, var);
    ++argIdx;
  }
  assert(argIdx == pass.loweredFnConv.getNumIndirectSILResults());
  return argIdx;
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

/// Is this operand composing an aggregate from a subobject, or simply
/// forwarding the operand's value to storage defined elsewhere?
///
/// TODO: Handle struct.
/// TODO: Make this a visitor.
bool OpaqueStorageAllocation::canProjectFrom(SILInstruction *innerVal,
                                             SILInstruction *composingUse) {
  if (!OptimizeOpaqueAddressLowering)
    return false;

  switch (composingUse->getKind()) {
  default:
    return false;
  case ValueKind::ApplyInst:
    // @in operands never need their own storage since they are non-mutating
    // uses. They simply reuse the storage allocated for their operand. So it
    // wouldn't make sense to "project" out of the apply argument.
    return false;
  case ValueKind::EnumInst:
    break;
  case ValueKind::InitExistentialOpaqueInst: {
    // Ensure that all opened archetypes are available at the inner value's
    // definition.
    auto *initExistential = cast<InitExistentialOpaqueInst>(composingUse);
    for (Operand &operand : initExistential->getTypeDependentOperands()) {
      if (!pass.domInfo->properlyDominates(operand.get(), innerVal))
        return false;
    }
    break;
  }
  case ValueKind::ReturnInst:
    return true;
  case ValueKind::StoreInst: {
    if (cast<StoreInst>(composingUse)->getSrc() == innerVal
        && isa<CopyValueInst>(innerVal)) {
      return true;
    }
    return false;
  }
  case ValueKind::TupleInst:
    break;
  }
  ValueStorage &storage = pass.valueStorageMap.getStorage(composingUse);
  if (SILValue addr = storage.storageAddress) {
    if (auto *stackInst = dyn_cast<AllocStackInst>(addr)) {
      assert(pass.domInfo->properlyDominates(stackInst, innerVal));
      return true;
    }
    if (isa<SILFunctionArgument>(addr)) {
      return true;
    }
  } else if (storage.isProjection())
    return canProjectFrom(innerVal, storage.getComposedOperand()->getUser());

  return false;
}

/// Allocate storage for a single opaque/resilient value.
void OpaqueStorageAllocation::allocateForValue(SILValue value,
                                               ValueStorage &storage) {
  assert(!isa<SILFunctionArgument>(value));

  if (ApplySite::isa(value)) {
    // Result tuples will be canonicalized during apply rewritting so the tuple
    // itself is unused.
    if (value->getType().is<TupleType>()) {
      assert(ApplySite(value).getSubstCalleeType()->getNumResults() > 1);
      return;
    }
  }

  // Argument loads already have a storage address.
  if (storage.storageAddress) {
    assert(isa<SILFunctionArgument>(storage.storageAddress));
    return;
  }

  if (value->hasOneUse()) {
    // TODO: Handle block arguments.
    // TODO: Handle subobjects with a single composition, and other non-mutating
    // uses such as @in arguments.
    if (SILInstruction *def = dyn_cast<SILInstruction>(value)) {
      Operand *useOper = *value->use_begin();
      if (canProjectFrom(def, useOper->getUser())) {
        storage.setComposedOperand(useOper);
        return;
      }
    }
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

//===----------------------------------------------------------------------===//
// AddressMaterialization - materialize storage addresses, generate projections.
//===----------------------------------------------------------------------===//

namespace {
/// Materialize the address of a value's storage. For values that are directly
/// mapped to a storage location, simply return the mapped `AllocStackInst`.
/// For subobjects emit any necessary `_addr` projections using the provided
/// `SILBuilder`.
///
/// This is a common utility for ApplyRewriter, AddressOnlyDefRewriter,
/// and AddressOnlyUseRewritter.
class AddressMaterialization {
  AddressLoweringState &pass;
  SILBuilder &B;

public:
  AddressMaterialization(AddressLoweringState &pass, SILBuilder &B)
      : pass(pass), B(B) {}

  SILValue initializeOperandMem(Operand *operand);

  SILValue materializeAddress(SILValue origValue);

protected:
  SILValue materializeProjection(Operand *operand);
};
} // anonymous namespace

// Materialize an address pointing to initialized memory for this operand,
// generating a projection and copy if needed.
SILValue AddressMaterialization::initializeOperandMem(Operand *operand) {
  SILValue def = operand->get();
  SILValue destAddr;
  if (operand->get()->getType().isAddressOnly(pass.F->getModule())) {
    ValueStorage &storage = pass.valueStorageMap.getStorage(def);
    // Source value should already be rewritten.
    assert(storage.isRewritten());
    if (storage.isProjection())
      destAddr = storage.storageAddress;
    else {
      destAddr = materializeProjection(operand);
      B.createCopyAddr(operand->getUser()->getLoc(), storage.storageAddress,
                       destAddr, IsTake, IsInitialization);
    }
  } else {
    destAddr = materializeProjection(operand);
    B.createStore(operand->getUser()->getLoc(), operand->get(), destAddr,
                  StoreOwnershipQualifier::Unqualified);
  }
  return destAddr;
}

/// Return the address of the storage for `origValue`. This may involve
/// materializing projections.
SILValue AddressMaterialization::materializeAddress(SILValue origValue) {
  ValueStorage &storage = pass.valueStorageMap.getStorage(origValue);

  if (!storage.storageAddress)
    storage.storageAddress =
        materializeProjection(storage.getComposedOperand());

  return storage.storageAddress;
}

SILValue AddressMaterialization::materializeProjection(Operand *operand) {
  SILInstruction *user = operand->getUser();

  switch (user->getKind()) {
  default:
    DEBUG(user->dump());
    llvm_unreachable("Unexpected subobject composition.");
  case ValueKind::EnumInst: {
    auto *enumInst = cast<EnumInst>(user);
    SILValue enumAddr = materializeAddress(enumInst);
    return B.createInitEnumDataAddr(enumInst->getLoc(), enumAddr,
                                    enumInst->getElement(),
                                    operand->get()->getType().getAddressType());
  }
  case ValueKind::InitExistentialOpaqueInst: {
    auto *initExistentialOpaque = cast<InitExistentialOpaqueInst>(user);
    SILValue containerAddr = materializeAddress(initExistentialOpaque);
    auto canTy = initExistentialOpaque->getFormalConcreteType();
    auto opaque = Lowering::AbstractionPattern::getOpaque();
    auto &concreteTL = pass.F->getModule().Types.getTypeLowering(opaque, canTy);
    return B.createInitExistentialAddr(
        initExistentialOpaque->getLoc(), containerAddr, canTy,
        concreteTL.getLoweredType(), initExistentialOpaque->getConformances());
  }
  case ValueKind::ReturnInst: {
    assert(pass.loweredFnConv.hasIndirectSILResults());
    return pass.F->getArguments()[0];
  }
  case ValueKind::TupleInst: {
    auto *tupleInst = cast<TupleInst>(user);
    // Function return values.
    if (tupleInst->hasOneUse()
        && isa<ReturnInst>(tupleInst->use_begin()->getUser())) {
      unsigned resultIdx = tupleInst->getElementIndex(operand);
      assert(resultIdx < pass.loweredFnConv.getNumIndirectSILResults());
      // Cannot call getIndirectSILResults here because that API uses the
      // original function type.
      return pass.F->getArguments()[resultIdx];
    }
    // TODO: emit tuple_element_addr
    llvm_unreachable("Unimplemented");
  }
  }
}

//===----------------------------------------------------------------------===//
// ApplyRewriter - rewrite call sites with indirect arguments.
//===----------------------------------------------------------------------===//

namespace {
/// Rewrite an Apply, lowering its indirect SIL arguments.
///
/// Replace indirect parameter arguments of this function with address-type
/// arguments.
///
/// Insert new indirect result arguments for this function to represent the
/// caller's storage.
class ApplyRewriter {
  AddressLoweringState &pass;
  ApplySite apply;
  SILBuilder argBuilder;

public:
  ApplyRewriter(SILInstruction *origCall, AddressLoweringState &pass)
      : pass(pass), apply(origCall), argBuilder(origCall) {}

  void rewriteParameters();
  void rewriteIndirectParameter(Operand *operand);

  void convertApplyWithIndirectResults();

protected:
  void
  canonicalizeResults(SmallVectorImpl<SILInstruction *> &directResultValues,
                      ArrayRef<Operand *> nonCanonicalUses);
  SILValue materializeIndirectResultAddress(SILInstruction *origDirectResultVal,
                                            SILType argTy);
};
}

/// Rewrite any indirect parameter in place.
void ApplyRewriter::rewriteParameters() {
  // Rewrite all incoming indirect operands.
  unsigned calleeArgIdx = apply.getCalleeArgIndexOfFirstAppliedArg();
  for (Operand &operand : apply.getArgumentOperands()) {
    if (operand.get()->getType().isObject()) {
      auto argConv =
          apply.getSubstCalleeConv().getSILArgumentConvention(calleeArgIdx);
      if (argConv.isIndirectConvention())
        rewriteIndirectParameter(&operand);
    }
    ++calleeArgIdx;
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

/// Rewrite a formally indirect parameter in place.
/// Update the operand to the incoming value's storage address.
/// After this, the SIL argument types no longer match SIL function conventions.
///
/// Temporary argument storage may be created for loadable values.
///
/// Note: Temporary argument storage does not own its value. If the argument
/// is owned, the stored value should already have been copied.
void ApplyRewriter::rewriteIndirectParameter(Operand *operand) {
  SILValue argValue = operand->get();

  if (argValue->getType().isAddressOnly(pass.F->getModule())) {
    ValueStorage &storage = pass.valueStorageMap.getStorage(argValue);
    // Source value should already be rewritten.
    assert(storage.isRewritten());
    operand->set(storage.storageAddress);
    return;
  }
  // Allocate temporary storage for a loadable operand.
  AllocStackInst *allocInstr =
      argBuilder.createAllocStack(apply.getLoc(), argValue->getType());

  argBuilder.createStore(apply.getLoc(), argValue, allocInstr,
                         StoreOwnershipQualifier::Unqualified);

  operand->set(allocInstr);

  insertStackDeallocationAtCall(allocInstr, apply.getInstruction(),
                                /*argLoad=*/nullptr);
}

// Canonicalize call result uses. Treat each result of a multi-result call as
// an independent value. Currently, SILGen may generate tuple_extract for each
// result but generate a single destroy_value for the entire tuple of
// results. This makes it impossible to reason about each call result as an
// independent value according to the callee's function type.
//
// directResultValues has an entry for each tuple extract corresponding to
// that result if one exists. This function will add an entry to
// directResultValues whenever it needs to materialize a TupleExtractInst.
void ApplyRewriter::canonicalizeResults(
    SmallVectorImpl<SILInstruction *> &directResultValues,
    ArrayRef<Operand *> nonCanonicalUses) {

  auto *applyInst = apply.getInstruction();

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

/// Return the storage address for the indirect result corresponding to the
/// given orignal result value. Allocate temporary argument storage for any
/// indirect results that are unmapped because they are loadable or unused.
///
/// origDirectResultVal may be nullptr for unused results.
SILValue ApplyRewriter::materializeIndirectResultAddress(
    SILInstruction *origDirectResultVal, SILType argTy) {

  if (origDirectResultVal
      && origDirectResultVal->getType().isAddressOnly(pass.F->getModule())) {
    auto &storage = pass.valueStorageMap.getStorage(origDirectResultVal);
    storage.markRewritten();
    // Pass the local storage address as the indirect result address.
    return storage.storageAddress;
  }
  // Allocate temporary call-site storage for an unused or loadable result.
  SILInstruction *origCallInst = apply.getInstruction();
  SILLocation loc = origCallInst->getLoc();
  auto *allocInst = argBuilder.createAllocStack(loc, argTy);
  LoadInst *loadInst = nullptr;
  if (origDirectResultVal) {
    // TODO: Find the try_apply's result block.
    // Build results outside-in to next stack allocations.
    SILBuilder resultBuilder(std::next(SILBasicBlock::iterator(origCallInst)));
    // This is a formally indirect argument, but is loadable.
    loadInst = resultBuilder.createLoad(loc, allocInst,
                                        LoadOwnershipQualifier::Unqualified);
    origDirectResultVal->replaceAllUsesWith(loadInst);
    pass.markDead(origDirectResultVal);
  }
  insertStackDeallocationAtCall(allocInst, origCallInst, loadInst);
  return SILValue(allocInst);
}

/// Allocate storage for formally indirect results at the given call site.
/// Create a new call instruction with indirect SIL arguments.
void ApplyRewriter::convertApplyWithIndirectResults() {
  assert(apply.getSubstCalleeType()->hasIndirectFormalResults());

  SILInstruction *origCallInst = apply.getInstruction();
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
    if (!nonCanonicalUses.empty())
      canonicalizeResults(origDirectResultValues, nonCanonicalUses);
  } else {
    // This call has a single, indirect result (convertApplyWithIndirectResults
    // only handles call with at least one indirect result).
    // An unused result can remain unmapped. Temporary storage will be allocated
    // later when fixing up the call's uses.
    assert(origDirectResultValues.size() == 1);
    if (!origCallInst->use_empty()) {
      assert(pass.valueStorageMap.contains(origCallInst));
      origDirectResultValues[0] = origCallInst;
    }
  }

  // Prepare to emit a new call instruction.
  SILLocation loc = origCallInst->getLoc();
  SILBuilder callBuilder(origCallInst);

  // The new call instruction's SIL calling convention.
  SILFunctionConventions loweredCalleeConv(
      apply.getSubstCalleeType(),
      SILModuleConventions::getLoweredAddressConventions());

  // The new call instruction's SIL argument list.
  SmallVector<SILValue, 8> newCallArgs(loweredCalleeConv.getNumSILArguments());

  // Map the original result indices to new result indices.
  SmallVector<unsigned, 8> newDirectResultIndices(
    origFnConv.getNumDirectSILResults());
  // Indices used to populate newDirectResultIndices.
  unsigned oldDirectResultIdx = 0, newDirectResultIdx = 0;

  // The index of the next indirect result argument.
  unsigned newResultArgIdx =
      loweredCalleeConv.getSILArgIndexOfFirstIndirectResult();

  // Visit each result. Redirect results that are now indirect by calling
  // materializeIndirectResultAddress.  Result that remain direct will be
  // redirected later.  Populate newCallArgs and newDirectResultIndices.
  for_each(
    apply.getSubstCalleeType()->getResults(),
    origDirectResultValues, 
    [&](SILResultInfo resultInfo, SILInstruction *origDirectResultVal) {
      // Assume that all original results are direct in SIL.
      assert(!origFnConv.isSILIndirect(resultInfo));

      if (loweredCalleeConv.isSILIndirect(resultInfo)) {
        SILValue indirectResultAddr = materializeIndirectResultAddress(
            origDirectResultVal, loweredCalleeConv.getSILType(resultInfo));
        // Record the new indirect call argument.
        newCallArgs[newResultArgIdx++] = indirectResultAddr;
        // Leave a placeholder for indirect results.
        newDirectResultIndices[oldDirectResultIdx++] = ~0;
      } else {
        // Record the new direct result, and advance the direct result indices.
        newDirectResultIndices[oldDirectResultIdx++] = newDirectResultIdx++;
      }
      // replaceAllUses will be called later to handle direct results that
      // remain direct results of the new call instruction.
    });

  // Append the existing call arguments to the SIL argument list. They were
  // already lowered to addresses by rewriteIncomingArgument.
  assert(newResultArgIdx == loweredCalleeConv.getSILArgIndexOfFirstParam());
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
        loweredCalleeConv.getSILResultType(), apply.getSubstitutions(),
        newCallArgs, cast<ApplyInst>(origCallInst)->isNonThrowing());
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
      // Uses of indirect results will be rewritten by AddressOnlyUseRewriter.
      assert(loweredCalleeConv.isSILIndirect(resultInfo));
      assert(pass.valueStorageMap.contains(extractInst));
      if (extractInst->use_empty())
        pass.markDead(extractInst);
      continue;
    }
    if (loweredCalleeConv.isSILIndirect(resultInfo)) {
      // This loadable indirect use should already be redirected to a load from
      // the argument storage and marked dead.
      assert(extractInst->use_empty());
      continue;
    }
    // Either the new call instruction has only a single direct result, or we
    // map the original tuple field to the new tuple field.
    SILInstruction *newValue = newCallInst;
    if (loweredCalleeConv.getNumDirectSILResults() > 1) {
      assert(newCallInst->getType().is<TupleType>());
      newValue = resultBuilder.createTupleExtract(
        extractInst->getLoc(), newCallInst,
        newDirectResultIndices[origResultIdx]);
    }
    extractInst->replaceAllUsesWith(newValue);
    extractInst->eraseFromParent();
  }
  if (!pass.valueStorageMap.contains(origCallInst))
    pass.markDead(origCallInst);
}

//===----------------------------------------------------------------------===//
// ReturnRewriter - rewrite return instructions for indirect results.
//===----------------------------------------------------------------------===//

class ReturnRewriter {
  AddressLoweringState &pass;

public:
  ReturnRewriter(AddressLoweringState &pass) : pass(pass) {}

  void rewriteReturns();

protected:
  void rewriteReturn(ReturnInst *returnInst);
};

void ReturnRewriter::rewriteReturns() {
  for (TermInst *termInst : pass.returnInsts) {
    // TODO: handle throws
    rewriteReturn(cast<ReturnInst>(termInst));
  }
}

void ReturnRewriter::rewriteReturn(ReturnInst *returnInst) {
  auto insertPt = SILBasicBlock::iterator(returnInst);
  auto bbStart = returnInst->getParent()->begin();
  while (insertPt != bbStart) {
    --insertPt;
    if (!isa<DeallocStackInst>(*insertPt))
      break;
  }
  SILBuilder B(insertPt);

  // Gather direct function results.
  unsigned numOrigDirectResults =
      pass.F->getConventions().getNumDirectSILResults();
  SmallVector<SILValue, 8> origDirectResultValues;
  if (numOrigDirectResults == 1)
    origDirectResultValues.push_back(returnInst->getOperand());
  else {
    auto *tupleInst = cast<TupleInst>(returnInst->getOperand());
    origDirectResultValues.append(tupleInst->getElements().begin(),
                                  tupleInst->getElements().end());
    assert(origDirectResultValues.size() == numOrigDirectResults);
  }

  SILFunctionConventions origFnConv(pass.F->getConventions());

  // Convert each result.
  SmallVector<SILValue, 8> newDirectResults;
  unsigned newResultArgIdx =
      pass.loweredFnConv.getSILArgIndexOfFirstIndirectResult();

  for_each(
      pass.F->getLoweredFunctionType()->getResults(), origDirectResultValues,
      [&](SILResultInfo resultInfo, SILValue origDirectResultVal) {
        // Assume that all original results are direct in SIL.
        assert(!origFnConv.isSILIndirect(resultInfo));

        if (pass.loweredFnConv.isSILIndirect(resultInfo)) {
          assert(newResultArgIdx
                 < pass.loweredFnConv.getSILArgIndexOfFirstParam());

          SILArgument *resultArg = B.getFunction().getArgument(newResultArgIdx);
          SILType resultTy = origDirectResultVal->getType();
          if (resultTy.isAddressOnly(pass.F->getModule())) {
            ValueStorage &storage =
                pass.valueStorageMap.getStorage(origDirectResultVal);
            assert(storage.isRewritten());
            if (!storage.isProjection()) {
              // Copy the result from local storage into the result argument.
              SILValue resultAddr = storage.storageAddress;
              B.createCopyAddr(returnInst->getLoc(), resultAddr, resultArg,
                               IsTake, IsInitialization);
            }
          } else {
            // Store the result into the result argument.
            B.createStore(returnInst->getLoc(), origDirectResultVal, resultArg,
                          StoreOwnershipQualifier::Unqualified);
          }
          ++newResultArgIdx;
        } else {
          // Record the direct result for populating the result tuple.
          newDirectResults.push_back(origDirectResultVal);
        }
      });
  assert(newDirectResults.size()
         == pass.loweredFnConv.getNumDirectSILResults());
  SILValue newReturnVal;
  if (newDirectResults.empty()) {
    SILType emptyTy = B.getModule().Types.getLoweredType(
        TupleType::getEmpty(B.getModule().getASTContext()));
    newReturnVal = B.createTuple(returnInst->getLoc(), emptyTy, {});
  } else if (newDirectResults.size() == 1) {
    newReturnVal = newDirectResults[0];
  } else {
    newReturnVal =
        B.createTuple(returnInst->getLoc(),
                      pass.loweredFnConv.getSILResultType(), newDirectResults);
  }
  SILValue origFullResult = returnInst->getOperand();
  returnInst->setOperand(newReturnVal);
  if (auto *fullResultInst = dyn_cast<SILInstruction>(origFullResult)) {
    if (fullResultInst->use_empty())
      pass.markDead(fullResultInst);
  }
}

//===----------------------------------------------------------------------===//
// AddressOnlyUseRewriter - rewrite opaque value uses.
//===----------------------------------------------------------------------===//

namespace {
class AddressOnlyUseRewriter
    : SILInstructionVisitor<AddressOnlyUseRewriter, void> {
  friend SILVisitor<AddressOnlyUseRewriter, void>;

  AddressLoweringState &pass;

  SILBuilder B;
  AddressMaterialization addrMat;

  Operand *currOper;

public:
  explicit AddressOnlyUseRewriter(AddressLoweringState &pass)
      : pass(pass), B(*pass.F), addrMat(pass, B) {}

  void visitOperand(Operand *operand) {
    currOper = operand;
    visit(operand->getUser());
  }

protected:
  void markRewritten(SILValue addr) {
    auto &storage = pass.valueStorageMap.getStorage(currOper->getUser());
    storage.storageAddress = addr;
    storage.markRewritten();
  }

  void beforeVisit(ValueBase *V) {
    DEBUG(llvm::dbgs() << "  REWRITE USE "; V->dump());

    auto *I = cast<SILInstruction>(V);
    B.setInsertionPoint(I);
    B.setCurrentDebugScope(I->getDebugScope());
  }

  void visitValueBase(ValueBase *V) {
    DEBUG(V->dump());
    llvm_unreachable("Unimplemented?!");
  }

  void visitApplyInst(ApplyInst *applyInst) {
    ApplyRewriter(applyInst, pass).rewriteIndirectParameter(currOper);
  }

  void visitCopyValueInst(CopyValueInst *copyInst) {
    ValueStorage &storage = pass.valueStorageMap.getStorage(copyInst);
    // Fold a copy into a store.
    if (storage.isProjection()
        && isa<StoreInst>(storage.getComposedOperand()->getUser())) {
      return;
    }
    SILValue srcVal = copyInst->getOperand();
    SILValue srcAddr = pass.valueStorageMap.getStorage(srcVal).storageAddress;
    SILValue destAddr = addrMat.materializeAddress(copyInst);
    B.createCopyAddr(copyInst->getLoc(), srcAddr, destAddr, IsNotTake,
                     IsInitialization);
    markRewritten(destAddr);
  }
  
  void visitDebugValueInst(DebugValueInst *debugInst) {
    SILValue srcVal = debugInst->getOperand();
    SILValue srcAddr = pass.valueStorageMap.getStorage(srcVal).storageAddress;
    B.createDebugValueAddr(debugInst->getLoc(), srcAddr);
    pass.markDead(debugInst);
  }
  
  void visitDestroyValueInst(DestroyValueInst *destroyInst) {
    SILValue srcVal = destroyInst->getOperand();
    SILValue srcAddr = pass.valueStorageMap.getStorage(srcVal).storageAddress;
    B.createDestroyAddr(destroyInst->getLoc(), srcAddr);
    pass.markDead(destroyInst);
  }

  // Handle EnumInst on the def side to handle both opaque and
  // loadable operands.
  void visitEnumInst(EnumInst *enumInst) {}

  // Handle InitExistentialOpaque on the def side to handle both opaque and
  // loadable operands.
  void
  visitInitExistentialOpaqueInst(InitExistentialOpaqueInst *initExistential) {}

  void visitReturnInst(ReturnInst *returnInst) {
    // Returns are rewritten for any function with indirect results after opaque
    // value rewriting.
  }

  void visitStoreInst(StoreInst *storeInst) {
    SILValue srcVal = storeInst->getSrc();
    assert(currOper->get() == srcVal);

    ValueStorage &storage = pass.valueStorageMap.getStorage(srcVal);
    SILValue srcAddr = storage.storageAddress;

    IsTake_t isTakeFlag = IsTake;
    assert(storeInst->getOwnershipQualifier()
           == StoreOwnershipQualifier::Unqualified);

    if (storage.isProjection()) {
      assert(!srcAddr);
      auto *copyInst = cast<CopyValueInst>(srcVal);
      ValueStorage &srcStorage =
          pass.valueStorageMap.getStorage(copyInst->getOperand());
      assert(!srcStorage.isProjection());
      srcAddr = srcStorage.storageAddress;
      isTakeFlag = IsNotTake;
    }
    // Bitwise copy the value. Two locations now share ownership. This is
    // modeled as a take-init.
    B.createCopyAddr(storeInst->getLoc(), srcAddr, storeInst->getDest(),
                     isTakeFlag, IsInitialization);
    pass.markDead(storeInst);
  }

  void visitTupleInst(TupleInst *tupleInst) {
    // Tuples are rewritten on the def-side, where both direct and indirect
    // elements are composed.
  }

  void visitTupleExtractInst(TupleExtractInst *extractInst) {
    // Apply results are rewritten when the result definition is visited.
    if (ApplySite::isa(currOper->get()))
      return;

    // TODO: generate tuple_element_addr.
    // generate copy_addr if we can't project.
    llvm_unreachable("unimplemented.");
  }
};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// AddressOnlyDefRewriter - rewrite opaque value definitions.
//===----------------------------------------------------------------------===//

namespace {
class AddressOnlyDefRewriter
    : SILInstructionVisitor<AddressOnlyDefRewriter, void> {
  friend SILVisitor<AddressOnlyDefRewriter, void>;

  AddressLoweringState &pass;

  SILBuilder B;
  AddressMaterialization addrMat;

  ValueStorage *storage = nullptr;

public:
  explicit AddressOnlyDefRewriter(AddressLoweringState &pass)
      : pass(pass), B(*pass.F), addrMat(pass, B) {}

  void visitInst(SILInstruction *inst) { visit(inst); }

protected:
  void beforeVisit(ValueBase *V) {
    storage = &pass.valueStorageMap.getStorage(V);

    DEBUG(llvm::dbgs() << "REWRITE DEF "; V->dump());
    if (storage->storageAddress)
      DEBUG(llvm::dbgs() << "  STORAGE "; storage->storageAddress->dump());

    auto *I = cast<SILInstruction>(V);
    B.setInsertionPoint(I);
    B.setCurrentDebugScope(I->getDebugScope());
  }

  void visitValueBase(ValueBase *V) {
    DEBUG(V->dump());
    llvm_unreachable("Unimplemented?!");
  }

  void visitApplyInst(ApplyInst *applyInst) {
    assert(!storage->isRewritten());
    // Completely rewrite the apply instruction, handling any remaining
    // (loadable) indirect parameters, allocating memory for indirect
    // results, and generating a new apply instruction.
    ApplyRewriter rewriter(applyInst, pass);
    rewriter.rewriteParameters();
    rewriter.convertApplyWithIndirectResults();
  }

  void visitCopyValueInst(CopyValueInst *copyInst) {
    // A folded copy is not rewritten.
    assert(storage->isProjection() || storage->isRewritten());
  }

  void visitEnumInst(EnumInst *enumInst) {
    SILValue enumAddr;
    if (enumInst->hasOperand()) {
      addrMat.initializeOperandMem(&enumInst->getOperandRef());

      assert(storage->storageAddress);
      enumAddr = storage->storageAddress;
    } else
      enumAddr = addrMat.materializeAddress(enumInst);

    B.createInjectEnumAddr(enumInst->getLoc(), enumAddr,
                           enumInst->getElement());

    storage->markRewritten();
  }

  void visitInitExistentialOpaqueInst(
      InitExistentialOpaqueInst *initExistentialOpaque) {

    // Initialize memory for the operand which may be opaque or loadable.
    addrMat.initializeOperandMem(&initExistentialOpaque->getOperandRef());

    assert(storage->storageAddress);
    storage->markRewritten();
  }

  void visitLoadInst(LoadInst *loadInst) {
    // Bitwise copy the value. Two locations now share ownership. This is
    // modeled as a take-init.
    SILValue addr = pass.valueStorageMap.getStorage(loadInst).storageAddress;
    if (addr != loadInst->getOperand()) {
      B.createCopyAddr(loadInst->getLoc(), loadInst->getOperand(), addr, IsTake,
                       IsInitialization);
    }
    storage->markRewritten();
  }

  void visitTupleInst(TupleInst *tupleInst) {
    ValueStorage &storage = pass.valueStorageMap.getStorage(tupleInst);
    if (storage.isProjection()
        && isa<ReturnInst>(storage.getComposedOperand()->getUser())) {
      // For indirectly returned values, each element has its own storage.
      return;
    }
    // For each element, initialize the operand's memory. Some tuple elements
    // may be loadable types.
    SILValue tupleAddr = addrMat.materializeAddress(tupleInst);
    unsigned eltIdx = 0;
    for (Operand &operand : tupleInst->getAllOperands()) {
      SILType eltTy = operand.get()->getType();
      if (eltTy.isAddressOnly(pass.F->getModule()))
        addrMat.initializeOperandMem(&operand);
      else {
        auto *elementAddr = B.createTupleElementAddr(
            tupleInst->getLoc(), tupleAddr, eltIdx, eltTy.getAddressType());
        B.createStore(tupleInst->getLoc(), operand.get(), elementAddr,
                      StoreOwnershipQualifier::Unqualified);
      }
      ++eltIdx;
    }
  }

  void visitTupleExtractInst(TupleExtractInst *extractInst) {
    // If the source is an opaque tuple, as opposed to a call result, then the
    // extract is rewritten on the use-side.
    if (storage->isRewritten())
      return;

    // This must be an indirect result for an apply that has not yet been
    // rewritten. Rewrite the apply.
    SILValue srcVal = extractInst->getOperand();
    assert(ApplySite::isa(srcVal));
    ApplyRewriter(cast<SILInstruction>(srcVal), pass)
        .convertApplyWithIndirectResults();

    assert(storage->storageAddress);
  }
};
} // end anonymous namespace

static void rewriteFunction(AddressLoweringState &pass) {
  AddressOnlyDefRewriter defVisitor(pass);
  AddressOnlyUseRewriter useVisitor(pass);

  for (auto &valueStorageI : pass.valueStorageMap) {
    SILValue valueDef = valueStorageI.first;
    if (auto *defInst = dyn_cast<SILInstruction>(valueDef))
      defVisitor.visitInst(defInst);

    SmallVector<Operand *, 8> uses(valueDef->getUses());
    for (Operand *oper : uses)
      useVisitor.visitOperand(oper);
  }

  // Rewrite any remaining (loadable) indirect parameters.
  for (SILInstruction *applyInst : pass.indirectApplies) {
    ApplySite apply(applyInst);
    // Calls with indirect formal results have already been rewritten.
    if (apply.getSubstCalleeType()->hasIndirectFormalResults()) {
      bool isRewritten = false;
      visitCallResults(apply, [&](SILInstruction *result) {
        if (result->getType().isAddressOnly(pass.F->getModule())) {
          assert(pass.valueStorageMap.getStorage(result).isRewritten());
          isRewritten = true;
          return false;
        }
        return true;
      });
      if (!isRewritten) {
        ApplyRewriter rewriter(applyInst, pass);
        rewriter.rewriteParameters();
        rewriter.convertApplyWithIndirectResults();
        continue;
      }
    }
    ApplyRewriter(applyInst, pass).rewriteParameters();
  }
  if (pass.F->getLoweredFunctionType()->hasIndirectFormalResults())
    ReturnRewriter(pass).rewriteReturns();
}

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
  auto *DA = PM->getAnalysis<DominanceAnalysis>();

  AddressLoweringState pass(F, DA->get(F));

  // Rewrite function args and insert alloc_stack/dealloc_stack.
  OpaqueStorageAllocation allocator(pass);
  allocator.allocateOpaqueStorage();

  DEBUG(llvm::dbgs() << "\nREWRITING: " << F->getName(); F->dump());

  // Rewrite instructions with address-only operands or results.
  rewriteFunction(pass);

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
