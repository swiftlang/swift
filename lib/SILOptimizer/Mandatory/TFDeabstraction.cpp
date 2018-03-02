//===--- TFDeabstraction.cpp - Lowering & canonicalization for tensor ops -===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This psss is in charge of lowering general code coming out of the mandatory
// SIL passes and producing a canonicalized and deabstracted standard form.
// It combines together standard techniques like inlining, generics
// specialization, and scalarization of structs and tuples.
//
// This is intended to be part of the mandatory passes, so its behavior is
// defined to be as simple and predictable as possible.  We don't want to use
// heuristic techniques to resolve virtual calls for example, we'd rather leave
// them, so the user has a simple and predictable model for what this can
// handle.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "tf-deabstraction"
#include "TFUtilities.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/AST/DiagnosticsSIL.h"
using namespace swift;
using namespace tf;

template<typename...T, typename...U>
static InFlightDiagnostic
diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag, U &&...args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

namespace {
  /// This class wraps the state and logic necessary to deabstract code into one
  /// specific SIL function, which has been designated as a potential top-level
  /// host for tensor code.
  class TFDeabstraction {
    SILFunction &fn;
    TensorFunctionClassifier &tfc;
    SILPassManager *passManager;

    /// This is the list of tensor operations in the current function, filled in
    /// by promoteIndirectTensorOperands.
    SmallVector<BuiltinInst*, 32> tensorOps;
  public:
    TFDeabstraction(SILFunction &fn, TensorFunctionClassifier &tfc,
                    SILPassManager *PM)
      : fn(fn), tfc(tfc), passManager(PM) {}

    /// Deabstract the specified top level function as a deabstraction context.
    void doIt();
  private:
    void inlineCalls();
    void simplifyTensorOperands();

    void promoteToSSA(MutableArrayRef<AllocStackInst*> allocs);
    void prepareStackAllocForPromotion(AllocStackInst *alloc);
    void propagateTensorOperands();
    void canonicalizeOps();
  };
}  // end anonymous namespace


/// Return true if this is a "array.uninitialized" call, which creates an array
/// and returns it with uninitialized elements for the caller to fill in.
static bool isArrayUninitialized(SILInstruction *call) {
  auto *apply = dyn_cast<ApplyInst>(call);
  if (!apply) return false;
  auto semantics = ArraySemanticsCall(apply, "array.uninitialized");
  return semantics.getKind() == ArrayCallKind::kArrayUninitialized;
}

/// Scan the function looking for call sites that should be inlined to expose
/// tensor operations, and inline them to expose those ops.  Also keep track of
/// whether we see any tensor ops - if not, there is no reason to further
/// process this function, so we can stop and exit early from other
/// canonicalizations.
void TFDeabstraction::inlineCalls() {
  // Use the mandatory inlining algorithm to expose call sites that contain
  // TensorHandle values as their argument or result lists.
  inlineForTFDeabstraction(fn,
     [&](FullApplySite site, const SILFunction &callee) -> bool {
       // Check for array internals which we could be inlined, but prefer to
       // leave in abstracted form for easier analysis.  For things like
       // Tensor<Float>([[1,2],[3,4]]), we prefer to see higher level array
       // construction calls beacuse we end up removing them anyway.
       if (isArrayUninitialized(site.getInstruction()))
         return false;

       // Get the type of the function being called after applying substitutions
       // at the call site.
       auto type = site.getSubstCalleeType();

       // If the call we found is to something that processes TensorHandles,
       // then we want it inlined.
       return tfc.containsTensorHandle(type);
     }
  );
}

/// Scan the operand list of the builtin.  If any operand is passed indirectly
/// (i.e., an address of a stack location is passed instead of the value itself)
/// then rewrite the builtin to use a loaded version of that value.
///
/// Similarly, if a primitive integer or floating point value is passed as a
/// struct value, extract out the underlying integer or float value.
///
static BuiltinInst *simplifyOperands(BuiltinInst *inst) {
  /// Return a VarDecl if this is a struct wrapping a single field which is a
  /// primitive integer or floating point value.  We accept multiple layers of
  /// struct wrappers as well, but return the decl for the top level field
  /// type.  This returns null in any other case.
  auto getPrimitiveStructField = [&](Type type) -> VarDecl* {
    VarDecl *result = nullptr;
    while (1) {
      auto decl = type->getAnyNominal();
      if (!decl || !isa<StructDecl>(decl)) return nullptr;

      // Check to see if there is a single stored field.
      auto fieldIt = decl->getStoredProperties().begin();
      if (fieldIt == decl->getStoredProperties().end()) return nullptr;

      // If this is the top level of the struct, retain the field decl.
      if (result == nullptr) result = *fieldIt;

      type = (*fieldIt++)->getType();
      if (fieldIt != decl->getStoredProperties().end()) return nullptr;

      // If we unwrapped a level and got to a builtin type, then this is a
      // wrapper.
      if (type->is<BuiltinIntegerType>() ||
          type->is<BuiltinFloatType>())
        return result;
    }
  };

  // Predicate that returns true if the specified type is an address type for
  // a loadable (non-address-only) value.
  auto isLoadableAddressType = [&](SILType type) -> bool {
    return type.isAddress() && type.isLoadable(inst->getModule());
  };

  // Predicate that returns true if an operand of the specified type should be
  // rewritten - either to load an address argument or expand a struct
  // parameter.
  auto canSimplifyOperand = [&](SILType type) -> bool {
    return isLoadableAddressType(type) ||
           getPrimitiveStructField(type.getSwiftRValueType()) != nullptr;
  };

  // If we don't have to change any operands, don't rewrite the builtin.
  bool mustChangeBuiltin = false;
  for (auto &op : inst->getAllOperands()) {
    if (canSimplifyOperand(op.get()->getType())) {
      mustChangeBuiltin = true;
      break;
    }
  }

  if (!mustChangeBuiltin) return inst;

  // Okay, we do have to simplify something.  Scan through and rewrite operands.
  SILBuilder B(inst);
  SmallVector<SILValue, 8> operands;
  for (auto &op : inst->getAllOperands()) {
    auto operand = op.get();
    // If this is an address operand, emit a load of the value.
    if (isLoadableAddressType(operand->getType())) {
      bool hasOwnership = inst->getFunction()->hasQualifiedOwnership();
      auto loadOwnership = hasOwnership ? LoadOwnershipQualifier::Trivial
                                        : LoadOwnershipQualifier::Unqualified;
      auto load = B.createLoad(inst->getLoc(), operand, loadOwnership);

      load->setDebugLocation(inst->getDebugLocation());
      operand = load;
    }

    // If this is a struct value, emit a struct extraction instruction.
    while (auto fieldDecl = getPrimitiveStructField(
                                     operand->getType().getSwiftRValueType())) {
      auto extract = B.createStructExtract(inst->getLoc(), operand, fieldDecl);
      extract->setDebugLocation(inst->getDebugLocation());
      operand = extract;
    }

    operands.push_back(operand);
  }

  // Now that we've rebuilt the operand list, create a new builtin and replace
  // the old one.
  auto *newInst =
    B.createBuiltin(inst->getLoc(), inst->getName(),
                    inst->getType(), /*no substitions*/{}, operands);
  newInst->setDebugLocation(inst->getDebugLocation());

  // Replace the old with the new and delete the old instruction.
  inst->replaceAllUsesPairwiseWith(newInst);
  inst->eraseFromParent();
  return newInst;
}

/// Identify all of the tensor operations in the current function, and scan them
/// to see if there are any indirect arguments, where the address of a stack
/// allocation is passed to the builtin.  These occur when the tensor op was in
/// a generic context and was passed a scalar attribute value of generic type.
///
/// If we find one of these indirect values, transform it into a load of the
/// address and a use of the loaded value.  This allows the stack allocation to
/// be promoted, allowing us to construct SSA def-use chains.
///
/// Similarly, if we see a Struct operand that wraps a primitive value, we
/// extract out the underlying scalar value until we get to a builtin integer or
/// floating point value.
///
/// Since we're scanning the function, keep track of all of the tensor
/// operations to avoid additional linear scans over the function.
///
void TFDeabstraction::simplifyTensorOperands() {
  for (auto &BB : fn) {
    for (auto I = BB.begin(), E = BB.end(); I != E; ) {
      // Manually move iterator to avoid invalidation if we replace 'inst'.
      auto *inst = &*I++;

      // Try to decode this instruction as an op.  If it isn't one, ignore it.
      if (auto opInfo = SILTensorOpInfo::decode(inst)) {
        // Simplify operands if possible.
        opInfo->inst = simplifyOperands(opInfo->inst);

        // Remember this for later passes.
        tensorOps.push_back(opInfo->inst);
      }
    }
  }
}

namespace {
  /// This helper is used to find stack allocations in the operand chains of
  /// tensor operations.  This operates on the pre-deabstraction code, so it has
  /// to be able to look through the various cases that will be eliminated
  /// later.
  class StackAllocFinder {
    SmallVectorImpl<AllocStackInst*> &stackAllocs;
    SmallPtrSet<SILInstruction*, 32> visited;
  public:

    StackAllocFinder(SmallVectorImpl<AllocStackInst*> &stackAllocs)
      : stackAllocs(stackAllocs) {}

    void run(ArrayRef<BuiltinInst*> tensorOps) {
      for (auto *op : tensorOps) {
        for (auto &operand : op->getAllOperands())
          findStackAllocsFromValue(operand.get());
      }
    }

  private:
    void findStackAllocsFromValue(SILValue value);
    void findStackAllocsFromLoadedAddress(SILValue pointer);
  };
} // end anonymous namespace

/// Scan the def-use chains of the specified operand value, looking through
/// operations that we can deabstract.  If we find stack allocations along the
/// way, add them to our set.
void StackAllocFinder::findStackAllocsFromValue(SILValue value) {
  // If we found a non-instruction operand, or an instruction we've already
  // visited, then we're done scanning it.
  auto *inst = value->getDefiningInstruction();
  if (!inst || !visited.insert(inst).second)
    return;

  // If this is one of the instructions we can deabstract by scalarizing, just
  // look through it.
  if (isa<TupleInst>(inst) || isa<StructInst>(inst) ||
      isa<StructExtractInst>(inst) || isa<TupleExtractInst>(inst)) {
    for (auto &operand : inst->getAllOperands())
      findStackAllocsFromValue(operand.get());
    return;
  }

  // If this is a load, then we can deabstract it if it is a SRoA'able pointer
  // to a stack allocation.
  if (auto *load = dyn_cast<LoadInst>(inst))
    findStackAllocsFromLoadedAddress(load->getOperand());
}

/// The specific pointer is being loaded by a tensor operation.  Recursively
/// process the pointer - if it is to a stack allocation that we can deabstract,
/// then recursively process any stores into it as values that feed the tensor
/// operation.
void StackAllocFinder::findStackAllocsFromLoadedAddress(SILValue pointer) {
  while (isa<TupleElementAddrInst>(pointer) ||
         isa<StructElementAddrInst>(pointer)) {
    pointer = cast<SingleValueInstruction>(pointer)->getOperand(0);
  }

  // If the base of the pointer is something other than a stack allocation or if
  // we already processed this, then we're done.
  auto *alloc = dyn_cast<AllocStackInst>(pointer);
  if (!alloc || !visited.insert(alloc).second) return;

  // Ok, we can handle this, remember it.
  stackAllocs.push_back(alloc);

  // Walk the use-def chains of the allocation, finding any stores that feed
  // into it, and recursively processing the values that are store into it.
  SmallVector<SILInstruction*, 4> instrsToProcess;
  instrsToProcess.push_back(alloc);

  while (!instrsToProcess.empty()) {
    auto *inst = instrsToProcess.pop_back_val();

    for (auto result : inst->getResults())
      for (auto use : result->getUses()) {
        auto *user = use->getUser();
        // If we bottomed out with a store instruction, and if the store is *to*
        // the alloc then we can recursively process the value stored into it.
        // Otherwise we keep chasing uses.
        if (auto *store = dyn_cast<StoreInst>(user)) {
          // If this is a store *to* the address, then process the stored value
          // as an input.
          if (use->getOperandNumber() == 1)
            findStackAllocsFromValue(store->getSrc());
          continue;
        }

        // If we bottomed out with a copy_addr into this address, then this is
        // a load of the other operand.
        if (auto *copyaddr = dyn_cast<CopyAddrInst>(user)) {
          if (use->getOperandNumber() == 1)
            findStackAllocsFromLoadedAddress(copyaddr->getSrc());
        }

        // If this is the original allocation or an SRoA'able projection of its
        // address, then recursively process users.
        if (isa<TupleElementAddrInst>(inst) ||
            isa<StructElementAddrInst>(inst)) {
          instrsToProcess.push_back(user);
          continue;
        }

        // Otherwise we don't know what kind of user this is, ignore it.
      }
  }
}

/// Scan the function looking for TensorHandle AllocStack instructions to
/// promote.
void TFDeabstraction::promoteToSSA(MutableArrayRef<AllocStackInst*> allocs) {
  // If there is nothing to promote, don't bother calculating dominator info.
  if (allocs.empty())
    return;

  // Do any necessary preprocessing of the stack allocations before promoting
  // them.
  for (auto alloc : allocs)
    prepareStackAllocForPromotion(alloc);

  // Otherwise the function does have tensor operations, so lets promote any
  // stack allocations out of the way so we can do simple dataflow analysis.
  auto domInfo = passManager->getAnalysis<DominanceAnalysis>()->get(&fn);
  promoteAllocsToSSA(allocs, domInfo);
}

/// Preprocess the specified allocation instruction to make it more suitable for
/// promotion to SSA.  In particularly, we eliminate CopyAddrInst and other
/// uses that could prevent us from promoting this.
void TFDeabstraction::prepareStackAllocForPromotion(AllocStackInst *alloc) {
  // TODO: We will eventually need to do real SRoA to handle the case when
  // we have tensor values mixed in with other random values that shouldn't
  // (or can't) be loaded.  For now, we can just fail to deabstract these
  // cases.  When we handle the general case, we should investigate whether
  // it makes sense to use TypeLowering as
  // AvailableValueDataflowContext::explodeCopyAddr does.
  auto &module = alloc->getModule();
  if (!alloc->getType().isLoadable(module))
    return;

  SmallVector<SILInstruction*, 4> usersToRemove;

  for (auto UI = alloc->use_begin(); UI != alloc->use_end(); ) {
    // See if this is an instruction we need to explode.
    auto *inst = (*UI++)->getUser();

    if (auto *copyAddr = dyn_cast<CopyAddrInst>(inst)) {
      // Lower the copy into a load and store + retain/release instructions.
      SILBuilder B(copyAddr);
      auto loc = copyAddr->getLoc();
      auto &TL = B.getTypeLowering(copyAddr->getSrc()->getType());

      SILValue value =
        TL.emitLoadOfCopy(B, loc, copyAddr->getSrc(), copyAddr->isTakeOfSrc());
      TL.emitStoreOfCopy(B, loc, value, copyAddr->getDest(),
                         copyAddr->isInitializationOfDest());

      usersToRemove.push_back(copyAddr);
      continue;
    }

    /// Turn a destroy_addr into a load+release_value pair.
    if (auto *destroy = dyn_cast<DestroyAddrInst>(inst)) {
      SILBuilder B(destroy);
      auto &lowering = B.getTypeLowering(destroy->getOperand()->getType());
      lowering.emitDestroyAddress(B, destroy->getLoc(), destroy->getOperand());
      usersToRemove.push_back(destroy);
      continue;
    }
  }

  for (auto dead : usersToRemove)
    dead->eraseFromParent();
}

/// The specified argument has tuple type that deabstraction needs to scalarize.
/// Explode it into its deabstracted elements, rebuilding it and the branch
/// instructions that feed it.  This returns a value of the original type that
/// can be used for further analysis.
static SILValue explodeSILTupleArgument(SILPHIArgument *arg) {
  SmallVector<SILValue, 4> newArgs;

  auto *argBB = arg->getParent();

  // Collect all the fields and add new BB arguments to the block for each of
  // them.
  auto tuple = arg->getType();
  unsigned numElements = tuple.castTo<TupleType>()->getNumElements();
  for (unsigned i = 0; i != numElements; ++i) {
    auto newArg = argBB->createPHIArgument(tuple.getTupleElementType(i),
                                           arg->getOwnershipKind());
    newArgs.push_back(newArg);
  }

  // Now that we have created all of the BB arguments, we can create a new
  // tuple inst, replace the old argument, and remove it.
  SILBuilder B(&argBB->front());
  auto replacement = B.createTuple(argBB->front().getLoc(),
                                   arg->getType(), newArgs);
  arg->replaceAllUsesWith(replacement);
  unsigned argNo = arg->getIndex();
  argBB->eraseArgument(argNo);

  // Ok, now that we've exploded the BB argument itself, we need to explode the
  // values passed in the predecessor blocks.
  for (auto pi : argBB->getPredecessorBlocks()) {
    auto *br = cast<BranchInst>(pi->getTerminator());
    SmallVector<SILValue, 8> operands;
    for (unsigned i = 0, e = br->getNumOperands(); i != e; ++i)
      if (i != argNo)
        operands.push_back(br->getOperand(i));

    auto origValue = br->getOperand(argNo);

    B.setInsertionPoint(br);

    // Add all of the extracted versions of the elements.
    for (unsigned i = 0; i != numElements; ++i)
      operands.push_back(B.createTupleExtract(br->getLoc(), origValue, i));

    // Replace the branch itself.
    SILBuilder(br).createBranch(br->getLoc(), br->getDestBB(), operands);
    br->eraseFromParent();
  }

  // Ok, we're done.  Return the generated StructInst that aggregates the
  // arguments back to the caller.
  return replacement;
}

/// The specified argument has struct type that deabstraction needs to
/// scalarize. Explode it into its deabstracted elements, rebuilding it and the
/// branch instructions that feed it.  This returns a value of the original type
/// that can be used for further analysis.
static SILValue explodeSILStructArgument(SILPHIArgument *arg) {
  SmallVector<VarDecl*, 4> elementDecls;
  SmallVector<SILValue, 4> newArgs;

  auto &M = arg->getFunction()->getModule();
  auto *argBB = arg->getParent();

  // Collect all the fields and add new BB arguments to the block for each of
  // them.
  auto structType = arg->getType();
  auto decl = structType.getStructOrBoundGenericStruct();
  for (auto fieldDecl : decl->getStoredProperties()) {
    elementDecls.push_back(fieldDecl);
    auto fieldTy = structType.getFieldType(fieldDecl, M);

    auto newArg = argBB->createPHIArgument(fieldTy, arg->getOwnershipKind());
    newArgs.push_back(newArg);
  }

  // Now that we have created all of the BB arguments, we can create a new
  // struct inst, replace the old argument, and remove it.
  SILBuilder B(&argBB->front());
  auto replacement = B.createStruct(argBB->front().getLoc(),
                                    arg->getType(), newArgs);
  arg->replaceAllUsesWith(replacement);
  unsigned argNo = arg->getIndex();
  argBB->eraseArgument(argNo);

  // Ok, now that we've exploded the BB argument itself, we need to explode the
  // values passed in the predecessor blocks.
  for (auto pi : argBB->getPredecessorBlocks()) {
    auto *br = cast<BranchInst>(pi->getTerminator());
    SmallVector<SILValue, 8> operands;
    for (unsigned i = 0, e = br->getNumOperands(); i != e; ++i)
      if (i != argNo)
        operands.push_back(br->getOperand(i));

    auto origValue = br->getOperand(argNo);

    B.setInsertionPoint(br);

    // Add all of the extracted versions of the elements.
    for (auto fieldDecl : elementDecls)
      operands.push_back(B.createStructExtract(br->getLoc(), origValue,
                                               fieldDecl));

    // Replace the branch itself.
    SILBuilder(br).createBranch(br->getLoc(), br->getDestBB(), operands);
    br->eraseFromParent();
  }

  // Ok, we're done.  Return the generated StructInst that aggregates the
  // arguments back to the caller.
  return replacement;
}

/// We've promoted any stack allocations that are in the way of tensor operands
/// so we now have proper SSA.  Look through struct and tuple injection and
/// projection instructions to find the underlying value that feeds the tensor
/// operation.  This is typically another tensor operation or a constant (for
/// attributes) but may be variables or other things that cause a send.
///
static SILValue
propagateTensorOperand(SILValue v,
                       SmallPtrSet<SILPHIArgument*, 8> &checkedPhis) {
  // This is the series of struct/tuple extract indices that the value is
  // currently being projected through.  Consider an access like this:
  //     B = struct { #1, #2 }
  //     C = tuple { #3, B }
  //     Y = tuple_extract C, 1
  //     Z = struct_extract Y, 0
  // We start analysis at Z, and add the access indices of Z and Y.  When we get
  // to C, we know that we're accessing element 1 from the tuple because that is
  // the top of our access path.  When we get to B, we know we're accessing
  // element 0 from the access path, so we return the #1 value.
  SmallVector<unsigned, 4> accessPath;

  SILValue lastRootValue;
  while (1) {
    // If our access path is empty, this is a candidate that we could return.
    if (accessPath.empty())
      lastRootValue = v;

    if (auto *arg = dyn_cast<SILPHIArgument>(v)) {
      // Don't reprocess a PHI argument if we've already seen it.
      if (!checkedPhis.insert(arg).second)
        break;

      // If this is an aggregate basic block argument, explode it into its
      // component values.
      if (!accessPath.empty()) {
        // We're going to erase 'arg', so don't leave dangling pointers in the
        // set.
        checkedPhis.erase(arg);
        if (arg->getType().is<TupleType>())
          v = explodeSILTupleArgument(arg);
        else if (arg->getType().is<StructType>() ||
                 arg->getType().is<BoundGenericStructType>())
          v = explodeSILStructArgument(arg);
        else
          break; // Cannot handle this.
        continue;
      }

      // Otherwise simplify inputs in predecessor blocks.
      for (auto pi : arg->getParent()->getPredecessorBlocks()) {
        auto *br = cast<BranchInst>(pi->getTerminator());
        // We intentionally recalculate arg->getIndex() because its index can
        // shift.  We know that recursive processing won't delete the bb arg
        // though, as it is in checkedPhis.
        auto incomingVal = br->getOperand(arg->getIndex());
        incomingVal = propagateTensorOperand(incomingVal, checkedPhis);
        br->setOperand(arg->getIndex(), incomingVal);
      }

      continue;
    }

    // Otherwise, peer through instructions.
    auto inst = v->getDefiningInstruction();
    if (!inst)
      break;

    // Extractions add to the access path.
    if (auto extract = dyn_cast<TupleExtractInst>(inst)) {
      accessPath.push_back(extract->getFieldNo());
      v = extract->getOperand();
      continue;
    }
    if (auto extract = dyn_cast<StructExtractInst>(inst)) {
      accessPath.push_back(extract->getFieldNo());
      v = extract->getOperand();
      continue;
    }

    // Constructions provide values to extract from if we have an access inside
    // of it.
    if (!accessPath.empty()) {
      if (auto str = dyn_cast<StructInst>(inst)) {
        v = str->getOperand(accessPath.pop_back_val());
        continue;
      }
      if (auto tuple = dyn_cast<TupleInst>(inst)) {
        v = tuple->getOperand(accessPath.pop_back_val());
        continue;
      }
    }

    // Otherwise, this is an unhandled instruction - we're done.
    break;
  }

  return lastRootValue;
}

/// Propagate the operand values for all tensors.
void TFDeabstraction::propagateTensorOperands() {
  SmallPtrSet<SILPHIArgument*, 8> checkedPhis;
  for (auto *op : tensorOps) {
    for (auto &operand : op->getAllOperands()) {

      // Get the propagated value.  This call can change the tensor operand.
      auto newVal = propagateTensorOperand(operand.get(), checkedPhis);
      // Get the (possibly-changed) instruction that used to be feeding the
      // tensor operation and set the new value.
      auto opInst = operand.get()->getDefiningInstruction();
      operand.set(newVal);

      // If the old instruction is unused, try to clean up the code.
      if (opInst && !opInst->hasUsesOfAnyResult())
        recursivelyDeleteTriviallyDeadInstructions(opInst);
    }
  }
}

/// Canonicalize tensor ops, validating their attribute arguments have
/// constants, and flattening array parameters.
void TFDeabstraction::canonicalizeOps() {
  for (auto &BB : fn) {
    for (auto I = BB.begin(), E = BB.end(); I != E; ) {
      // Manually move iterator to avoid invalidation if we replace 'inst'.
      auto *inst = &*I++;

      // If this is a well known function that can be transformed into an op, do
      // so first.
      if (auto apply = dyn_cast<ApplyInst>(inst))
        if (auto fn = apply->getCalleeFunction())
          inst = SILTensorOpInfo::decodeApply(apply, fn->getName());

      // Try to decode this instruction as an op.  If it isn't one, ignore it.
      auto opInfo = SILTensorOpInfo::decode(inst);
      if (!opInfo)
        continue;

      // Check to see if the usage of this op looks ok.  If not, reject it with
      // an error and ignore it.
      auto error = opInfo->checkAndDiagnoseOperands();
      if (!error.empty()) {
        // TODO: improve the diagnostic to talk about the parameter label in the
        // user code, not the internal op attribute.  The bookkeeping for this
        // isn't obvious though.
        auto loc = getUserSourceLocation(inst);
        diagnose(fn.getModule().getASTContext(), loc.getSourceLoc(),
                 diag::tf_op_misuse, error)
          .highlight(loc.getSourceRange());
        continue;
      }

      // If the tensor operation uses array parameters or has scalar values that
      // are passed through memory, promote them to being simple arguments to
      // make all subsequent analyses and promotion of the tensor operations
      // simpler.
      opInfo->canonicalizeOperands();
    }
  }
}

/// Process the specified top level function as a deabstraction context: if it
/// contains Tensor operations simplify the code using predictable rules until
/// the tensor operations are exposed in a canonical form inside of this
/// function.
///
/// We currently make use of the following techniques to do this:
///   1) Inlining.  We look for direct calls to functions that take and return
///      values of TensorHandle type, possibly wrapped by structs and tuples.
///   2) SSA Promotion of TensorHandle's.
///
/// TODO:
///   0) Move tensor op canonicalization up from tf-partition.
///   1) Generics specialization.
///   2) Struct and tuple scalarization.
///   3) Enums.  What can we reliably do with them?  Should they be out of
///      model?  We can definitely do ones without payload values.
///
void TFDeabstraction::doIt() {
  if (shouldDumpIntermediates()) {
    llvm::outs() << "--- TFDeabstraction Input: " << fn.getName() << "\n";
    fn.print(llvm::outs());
    llvm::outs() << "----\n";
  }

  // Start by inlining functions that take and return Tensor values.
  inlineCalls();

  // Scan for any Tensor operations, removing indirect operands and structs that
  // interfere with SSA construction.
  simplifyTensorOperands();

  // If we didn't find any ops, early exit processing of this function to save
  // compile time.
  if (tensorOps.empty())
    return;

  // Scan over all of the operands to the tensor ops, performing deabstraction
  // on each of them as necessary, finding stack allocations, struct and
  // tuple abstractions, function calls, etc.
  SmallVector<AllocStackInst*, 16> stackAllocs;
  StackAllocFinder(stackAllocs).run(tensorOps);

  // Promote stack allocations to SSA, this allows us to do dataflow analysis,
  // and eliminates mutation from tensor values.
  promoteToSSA(stackAllocs);

  // Now that we've promoted all the allocations in the way of our dataflow,
  // go through and propagate any tuple/struct values that are in the way of
  // our analysis.
  propagateTensorOperands();

#if 0 // Not ready yet.
  // Canonicalize tensor ops, validating their attribute arguments have
  // constants, and flattening array parameters.
  canonicalizeOps();
#endif

  if (shouldDumpIntermediates()) {
    llvm::outs() << "--- TFDeabstraction Result: " << fn.getName() << "\n";
    fn.print(llvm::outs());
    llvm::outs() << "----\n";
  }
}


namespace {
  class TFDeabstractionPass : public SILModuleTransform {
    TensorFunctionClassifier tfc;
  public:

    /// The entry point to the transformation, runs deabstraction on an entire
    /// module.
    void run() override;
  };
}  // end anonymous namespace

void TFDeabstractionPass::run() {
  SILModule *module = getModule();
  auto &ctx = module->getASTContext();

  // If the TensorFlow module hasn't been imported by the program, don't do
  // anything.  This avoids impacting compile time for non-TensorFlow using
  // Swift programs by doing extraneous analysis.
  auto tfModule = ctx.getLoadedModule(ctx.getIdentifier("TensorFlow"));
  if (!tfModule)
    return;

  // Loop over all of the functions in the current module processing them -
  // iff they look like they could be the top level of a deabstraction
  // context.
  for (auto &fn : *module) {
    // If this function is a building block of larger tensor programs (e.g.
    // the ops defined in the TensorFlow module), then don't transform it in
    // isolation.
    if (!tfc.shouldBePartitioned(&fn))
      continue;

    TFDeabstraction(fn, tfc, PM).doIt();

    // TODO(clattner): This should eventually be the driver that kicks off
    // the partitioning pass as part of it, and the partitioning and later
    // passes are just function passes that are invoked by this one.  Until
    // we are ready for that, let them run later in the pipeline after the
    // other optimization and cleanup passes.
  }
}

SILTransform *swift::createTFDeabstraction() {
  return new TFDeabstractionPass();
}
