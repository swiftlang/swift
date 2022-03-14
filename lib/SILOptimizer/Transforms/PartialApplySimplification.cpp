//===--- PartialApplySimplification.cpp - Lower partial applications ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// Reduces all partial application functions into explicit closure
/// constructions.
///
/// \c partial_apply is a useful high-level representation for optimization
/// passes like inlining, but it abstracts over many details of how closures
/// are constructed. In order to make IRGen lowering simpler, and provide some
/// opportunity for other passes to optimize closure construction.
///
/// When a closure implementation function is private, and is only referenced by
/// partial applications all of the same shape, then we can replace the function
/// with one that takes a closure box instead of the partially applied
/// arguments. Otherwise, a partial application forwarder function is generated
/// as a shim between the closure entry point, which takes the box, and the
/// original function, which takes the loaded arguments.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-partial-apply-simplification"

#include "llvm/Support/Debug.h"
#include "llvm/ADT/Statistic.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"

STATISTIC(NumInvocationFunctionsChanged,
          "Number of invocation functions rewritten");
STATISTIC(NumUnsupportedChangesToInvocationFunctions,
          "Number of invocation functions that could be rewritten, but aren't yet");
STATISTIC(NumPartialApplyCalleesWithNonPartialApplyUses,
          "Number of invocation functions with non-partial_apply uses");
STATISTIC(NumPartialApplyCalleesPossiblyUsedExternally,
          "Number of invocation functions possibly used externally");
STATISTIC(NumPartialApplyCalleesDeclarationOnly,
          "Number of invocation functions that are declaration-only");
STATISTIC(NumPartialApplyCalleesWithMismatchedPartialApplies,
          "Number of invocation functions that have mismatched partial_apply sites");
STATISTIC(NumDynamicPartialApplicationForwarders,
          "Number of dynamic partial application forwarder thunks generated");

using namespace swift;

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

struct KnownCallee {
  /// The set of function_refs to the callee.
  llvm::SetVector<FunctionRefInst *> FunctionRefs;
  /// The set of partial application sites.
  llvm::SetVector<PartialApplyInst *> PartialApplications;
  /// If the callee has a non-partial-apply use, this points to an arbitrary one.
  SILInstruction *NonPartialApplyUse = nullptr;
};

class PartialApplySimplificationPass : public SILModuleTransform {
  /// The entry point to the transformation.
  void run() override {
    // Scan all partial applications in the module so we know what to work with.
    llvm::DenseMap<SILFunction *, KnownCallee> knownCallees;
    llvm::SetVector<swift::PartialApplyInst *> dynamicCallees;
    for (auto &f : *getModule()) {
      scanFunction(&f, knownCallees, dynamicCallees);
    }

    for (auto &knownCallee : knownCallees) {
      processKnownCallee(knownCallee.first, knownCallee.second);
    }
    
    for (auto *dynamicPA : dynamicCallees) {
      processDynamicCallee(dynamicPA);
    }
  }

  void scanFunction(SILFunction *f,
                    llvm::DenseMap<SILFunction *,
                                   KnownCallee> &knownCallees,
                    llvm::SetVector<PartialApplyInst *> &dynamicCallees);
  
  void processKnownCallee(SILFunction *callee,
                          const KnownCallee &pa);
  
  void processDynamicCallee(PartialApplyInst *pa);
};

}

/// True if the partial application is in a form that can be trivially
/// lowered.
///
/// This is true if:
/// - the callee has convention(method)
/// - one argument is applied
/// - the callee is either not generic, or can read its generic environment
///   out of the single applied argument
/// - if the partial application is noescape:
///   - the argument is word-sized or smaller
///   - the argument is either trivial, or passed with a +0 convention
///     (guaranteed, unowned, in_guaranteed)
/// - if the partial application is escapable:
///   - the argument is either a single Swift-refcounted word, or trivial and
///     sized strictly less than one word
///   - the argument ownership convention matches the callee convention of the
///     resulting function
static bool isSimplePartialApply(PartialApplyInst *i) {
  auto calleeTy = i->getCallee()->getType().castTo<SILFunctionType>();
  if (calleeTy->getRepresentation() != SILFunctionTypeRepresentation::Method) {
    return false;
  }

  // TODO: could discount empty captured values here
  if (i->getNumArguments() != 1) {
    return false;
  }

  auto argTy = i->getArguments()[0]->getType();

  if (i->getFunctionType()->isNoEscape()) {
    if (argTy.isAddress()) {
      return true;
    }
    return false;
  } else {
    if (!argTy.isObject()) {
      return false;
    }
    
    // TODO: Handle native-refcounted classes, single-refcounted aggregates,
    // and bit-packable trivial types using knowledge from IRGen if this becomes
    // an IRGenPrepare pass
    if (!argTy.is<SILBoxType>()) {
      return false;
    }
  }
  
  return true;
}

void PartialApplySimplificationPass::scanFunction(SILFunction *f,
                         llvm::DenseMap<SILFunction *,
                                        KnownCallee> &knownCallees,
                         llvm::SetVector<PartialApplyInst *> &dynamicCallees) {
  // Consider all partial_apply instructions.
  for (auto &block : *f) {
    for (auto &inst : block) {
      // Examine the uses of static function refs.
      if (auto *fr = dyn_cast<FunctionRefInst>(&inst)) {
        auto &knownCallee = knownCallees[fr->getReferencedFunction()];
        knownCallee.FunctionRefs.insert(fr);
        
        for (auto *frUse : fr->getUses()) {
          // Collect partial applications for further transformation.
          if (auto pa = dyn_cast<PartialApplyInst>(frUse->getUser())) {
            knownCallee.PartialApplications.insert(pa);
            continue;
          }
          
          // Record if the function has uses that aren't partial applies.
          knownCallee.NonPartialApplyUse = frUse->getUser();
        }
      }
      
      if (auto *pa = dyn_cast<PartialApplyInst>(&inst)) {
        // Static callees get handled when we see the function_ref.
        if (isa<FunctionRefInst>(pa->getCallee())) {
          continue;
        }

        // If the callee isn't static, then we'll need to create a dynamic
        // forwarder thunk to simplify this partial application.
        dynamicCallees.insert(pa);
      }
    }
  }
}

void PartialApplySimplificationPass::processKnownCallee(SILFunction *callee,
                                                        const KnownCallee &pa) {
  auto &C = callee->getASTContext();
  
  // Skip functions with no partial application uses.
  if (pa.PartialApplications.empty())
    return;

  LLVM_DEBUG(llvm::dbgs() << "***** Processing known partial_apply callee "
                          << callee->getName() << " *****\n");
  
  // If the subject of the partial application has other uses that aren't
  // partial applications, then thunk it.
  if (pa.NonPartialApplyUse) {
    LLVM_DEBUG(llvm::dbgs() << "Callee has non-partial_apply uses; thunking\n";
               pa.NonPartialApplyUse->print(llvm::dbgs()));
    ++NumPartialApplyCalleesWithNonPartialApplyUses;
    goto create_forwarding_thunks;
  }

  // If the subject of the partial application might have external references,
  // or is itself an external reference, we can't change the existing function
  // signature. We'll always use forwarding thunks in this case.
  if (callee->isPossiblyUsedExternally()) {
    LLVM_DEBUG(llvm::dbgs() << "Callee is possibly used externally; thunking\n");
    ++NumPartialApplyCalleesPossiblyUsedExternally;
    goto create_forwarding_thunks;
  }
  if (callee->empty()) {
    LLVM_DEBUG(llvm::dbgs() << "Callee is a declaration only; thunking\n");
    ++NumPartialApplyCalleesDeclarationOnly;
    goto create_forwarding_thunks;
  }
  
  // Look at the set of all partial applications on this callee to figure
  // out what to do.
  // If all of the partial applications are identical (same number of arguments,
  // same convention, same escapiness, etc.), then we'll alter the invocation
  // function directly (or leave it alone, if the partial apply is simple
  // enough already.)
  
  // Take an arbitrary partial application as an example to compare the others.
  {
    auto examplePA = pa.PartialApplications.front();
    for (auto i = pa.PartialApplications.begin() + 1,
              e = pa.PartialApplications.end();
         i != e;
         ++i) {
      auto thisPA = *i;
      if (examplePA->getNumArguments() != thisPA->getNumArguments()
          || examplePA->getFunctionType()->getCalleeConvention()
              != thisPA->getFunctionType()->getCalleeConvention()
          || !examplePA->getFunctionType()->getExtInfo()
              .isEqualTo(thisPA->getFunctionType()->getExtInfo(), true)) {
        LLVM_DEBUG(llvm::dbgs() << "Mismatched partial application arguments; thunking:\n";
                   thisPA->print(llvm::dbgs());
                   examplePA->print(llvm::dbgs()));
        ++NumPartialApplyCalleesWithMismatchedPartialApplies;
        goto create_forwarding_thunks;
      }
    }
    
    // OK, all the partial applications look the same.
    LLVM_DEBUG(llvm::dbgs() << "All partial applications look like this:\n";
               examplePA->print(llvm::dbgs()));
    
    // If they're simple already, then we don't need to do anything.
    if (isSimplePartialApply(examplePA)) {
      LLVM_DEBUG(llvm::dbgs() << "And they're already simple, don't need to do anything!\n");
      return;
    }
    
    // Rewrite the function type to take the captures in box form.
    auto origTy = callee->getLoweredFunctionType();
    auto paResultTy = cast<SILFunctionType>(examplePA->getType().getASTType());
    // The box captures the generic context and the values of the arguments that
    // were partially applied. The invocation function is modified to take
    // a single partially-applied argument for the box, and unload the
    // elements of the box inside the function.
    SmallVector<SILField, 4> boxFields;
    
    unsigned numUnapplied
      = origTy->getParameters().size() - examplePA->getArguments().size();
    auto partiallyAppliedParams = origTy->getParameters().slice(numUnapplied);
    for (auto param : partiallyAppliedParams) {
      switch (param.getConvention()) {
      // Conventions where a copy of the argument is captured.
      case ParameterConvention::Direct_Guaranteed:
      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_In_Constant:
      case ParameterConvention::Indirect_In_Guaranteed:
        boxFields.push_back(SILField(param.getInterfaceType(), /*mutable*/false));
        break;
      
      // Conventions where an address to the argument is captured.
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_InoutAliasable:
        // Put a RawPointer in the box, which we can turn back into an address
        // in the function
        boxFields.push_back(SILField(C.TheRawPointerType, /*mutable*/false));
        break;
      }
    }
    
    // The new signature carries over the unapplied arguments.
    SmallVector<SILParameterInfo, 4> newParams;
    for (unsigned i = 0; i < numUnapplied; ++i) {
      newParams.push_back(origTy->getParameters()[i]);
    }
    
    // Instead of the applied arguments, we receive a box containing the
    // values for those arguments. Work out what that box type is.
    // TODO: We need a representation of boxes that
    // capture the generic environment to represent partial applications in
    // their full generality.
    if (origTy->getInvocationGenericSignature()) {
      LLVM_DEBUG(llvm::dbgs() << "TODO: generic partial_apply not yet implemented\n");
      ++NumUnsupportedChangesToInvocationFunctions;
      return;
    }

    // TODO: SILBoxType is only implemented for a single field right now, and we
    // don't yet have a corresponding type for nonescaping captures, so
    // represent the captures as a tuple for now.
    llvm::SmallVector<TupleTypeElt, 4> tupleElts;
    for (auto field : boxFields) {
      tupleElts.push_back(TupleTypeElt(field.getLoweredType()));
    }
    auto tupleTy = TupleType::get(tupleElts, C)->getCanonicalType();
    
    CanType contextTy;
    SILParameterInfo contextParam;
    
    bool isNoEscape = examplePA->getFunctionType()->isNoEscape();
    if (isNoEscape) {
      contextTy = tupleTy;
      // Nonescaping closures borrow their context from the outer frame.
      contextParam = SILParameterInfo(contextTy,
                                   ParameterConvention::Indirect_In_Guaranteed);
    } else {
      SILField tupleField(tupleTy, /*mutable*/ false);
      auto newBoxLayout = SILLayout::get(C,
                                         origTy->getInvocationGenericSignature(),
                                         tupleField,
                                         /*capturesGenerics*/ false);
      SubstitutionMap identitySubstitutionMap;
      if (auto origSig = origTy->getInvocationGenericSignature()) {
        identitySubstitutionMap = origSig->getIdentitySubstitutionMap();
      }
      contextTy = SILBoxType::get(C, newBoxLayout, identitySubstitutionMap);
      contextParam = SILParameterInfo(contextTy,
                                      paResultTy->getCalleeConvention());
    }
    
    newParams.push_back(contextParam);
    
    auto newExtInfo = origTy->getExtInfo()
      .withRepresentation(SILFunctionTypeRepresentation::Method);
    
    auto newTy = SILFunctionType::get(origTy->getInvocationGenericSignature(),
                                      newExtInfo, origTy->getCoroutineKind(),
                                      origTy->getCalleeConvention(),
                                      newParams,
                                      origTy->getYields(),
                                      origTy->getResults(),
                                      origTy->getOptionalErrorResult(),
                                      origTy->getPatternSubstitutions(),
                                      origTy->getInvocationSubstitutions(),
                                      C);
    
    LLVM_DEBUG(llvm::dbgs() << "Changing invocation function signature to\n";
               newTy->print(llvm::dbgs());
               llvm::dbgs() << '\n');
    
    // Change the invocation function to use the new type, and unbox the
    // captures in its entry block.
    callee->rewriteLoweredTypeUnsafe(newTy);
    
    // Update the entry block.
    {
      SILBuilder B(*callee);
      auto &entry = *callee->begin();
      
      // Insert an argument for the context before the originally applied args.
      auto contextArgTy = callee->mapTypeIntoContext(
                                   SILType::getPrimitiveObjectType(contextTy));
      if (isIndirectFormalParameter(contextParam.getConvention())) {
        contextArgTy = contextArgTy.getAddressType();
      }
      
      ValueOwnershipKind contextOwnership(*callee, contextArgTy,
                           SILArgumentConvention(contextParam.getConvention()));

      auto numUnappliedArgs = numUnapplied + origTy->getNumIndirectFormalResults();
      
      auto contextArg = entry.insertFunctionArgument(numUnappliedArgs,
                                                 contextArgTy,
                                                 contextOwnership);
      auto appliedBBArgs = entry.getArguments().slice(numUnappliedArgs + 1);

      // Replace the original arguments applied by the partial_apply, by
      // projections out of the box.
      SmallVector<AllocStackInst *, 4> AddedStackAllocs;
      B.setInsertionPoint(&entry, entry.begin());
      auto loc = examplePA->getLoc();
      for (unsigned i = 0; i < appliedBBArgs.size(); ++i) {
        auto appliedArg = appliedBBArgs[i];
        auto param = partiallyAppliedParams[i];

        SILValue proj;
        if (isNoEscape) {
          proj = contextArg;
        } else {
          proj = B.createProjectBox(loc, contextArg, 0);
        }
        if (boxFields.size() > 1) {
          proj = B.createTupleElementAddr(loc, proj, i);
        }
        // Load the value out of the context according to the current ownership
        // mode of the function and the calling convention for the parameter.
        SILValue projectedArg;
        if (callee->hasOwnership()) {
          switch (auto conv = param.getConvention()) {
          case ParameterConvention::Direct_Unowned:
            // Load an unowned image of the value from the box.
            projectedArg = B.createLoadUnowned(loc, proj, IsNotTake);
            break;
          case ParameterConvention::Direct_Guaranteed:
            // Load a borrow of the value from the box.
            projectedArg = B.createLoadBorrow(loc, proj);
            break;
          case ParameterConvention::Direct_Owned:
            // Load a copy of the value from the box.
            projectedArg = B.createLoad(loc, proj, LoadOwnershipQualifier::Copy);
            break;
          case ParameterConvention::Indirect_In:
          case ParameterConvention::Indirect_In_Constant: {
            // Allocate space for a copy of the value that can be consumed by the
            // function body. We'll need to deallocate the stack slot after the
            // cloned body.
            auto copySlot = B.createAllocStack(loc,
                                              proj->getType().getAddressType());
            AddedStackAllocs.push_back(copySlot);
            B.createCopyAddr(loc, proj, copySlot, IsNotTake, IsInitialization);
            projectedArg = copySlot;
            break;
          }
          case ParameterConvention::Indirect_In_Guaranteed:
            // We can borrow the value in-place in the box.
            projectedArg = proj;
            break;
          case ParameterConvention::Indirect_Inout:
          case ParameterConvention::Indirect_InoutAliasable: {
            // The box capture is a RawPointer with the value of the capture
            // address.
            auto ptrVal = B.createLoad(loc, proj, LoadOwnershipQualifier::Trivial);
            projectedArg = B.createPointerToAddress(loc, ptrVal,
                        appliedArg->getType(),
                        /*strict*/ conv == ParameterConvention::Indirect_Inout);
            break;
          }
          }
        } else {
          switch (auto conv = param.getConvention()) {
          case ParameterConvention::Direct_Unowned:
            // Load an unowned image of the value from the box.
            projectedArg = B.createLoad(loc, proj, LoadOwnershipQualifier::Unqualified);
            break;
          case ParameterConvention::Direct_Guaranteed:
            // Load a borrow of the value from the box.
            projectedArg = B.createLoad(loc, proj, LoadOwnershipQualifier::Unqualified);
            break;
          case ParameterConvention::Direct_Owned:
            // Load a copy of the value from the box.
            projectedArg = B.createLoad(loc, proj, LoadOwnershipQualifier::Unqualified);
            B.createRetainValue(loc, projectedArg, Atomicity::Atomic);
            break;
          case ParameterConvention::Indirect_In:
          case ParameterConvention::Indirect_In_Constant: {
            // Allocate space for a copy of the value that can be consumed by the
            // function body. We'll need to deallocate the stack slot after the
            // cloned body.
            auto copySlot = B.createAllocStack(loc,
                                               proj->getType().getAddressType());
            AddedStackAllocs.push_back(copySlot);
            B.createCopyAddr(loc, proj, copySlot, IsNotTake, IsInitialization);
            projectedArg = copySlot;
            break;
          }
          case ParameterConvention::Indirect_In_Guaranteed:
            // We can borrow the value in-place in the box.
            projectedArg = proj;
            break;
          case ParameterConvention::Indirect_Inout:
          case ParameterConvention::Indirect_InoutAliasable: {
            // The box capture is a RawPointer with the value of the capture
            // address.
            auto ptrVal = B.createLoad(loc, proj, LoadOwnershipQualifier::Unqualified);
            projectedArg = B.createPointerToAddress(loc, ptrVal,
                        appliedArg->getType(),
                        /*strict*/ conv == ParameterConvention::Indirect_Inout);
            break;
          }
          }
        }
        
        // Replace the original bb arg with the applied arg.
        appliedArg->replaceAllUsesWith(projectedArg);
      }
      
      // If the box is callee-consumed, we can release it now.
      if (contextParam.getConvention() == ParameterConvention::Direct_Owned) {
        if (callee->hasOwnership()) {
          B.createDestroyValue(loc, contextArg);
        } else {
          B.createStrongRelease(loc, contextArg, Atomicity::Atomic);
        }
      }
      
      // Erase the original applied arguments.
      for (unsigned i = 0; i < appliedBBArgs.size(); ++i) {
        entry.eraseArgument(numUnappliedArgs + 1);
      }
      
      // If we needed to introduce any stack slots to consume copies of
      // Indirect_In arguments, then balance them with deallocations on all
      // function exits.
      if (!AddedStackAllocs.empty()) {
        llvm_unreachable("todo");
      }
    }
    
    // Rewrite partial applications to partially apply the new clone.
    for (auto pa : pa.PartialApplications) {
      auto caller = pa->getFunction();
      SILBuilder B(*caller);
      auto loc = pa->getLoc();
      B.setInsertionPoint(pa);
      
      auto newFunctionRef = B.createFunctionRef(loc, callee);
      SILValue contextBuffer, contextProj;
      auto contextStorageTy = SILType::getPrimitiveAddressType(contextTy)
        .subst(getModule()->Types, pa->getSubstitutionMap());
      if (isNoEscape) {
        auto contextAlloc = B.createAllocStack(loc, contextStorageTy);
        contextBuffer = contextProj = contextAlloc;
        
        // We'll need to deallocate the context buffer after the end of the
        // original partial_apply's lifetime.
        auto deallocStackUses = pa->getUsersOfType<DeallocStackInst>();
        assert(deallocStackUses.begin() != deallocStackUses.end());
        for (auto use : deallocStackUses) {
          B.setInsertionPoint(use->getNextInstruction());
          B.createDeallocStack(loc, contextBuffer);
        }
        B.setInsertionPoint(contextAlloc->getNextInstruction());
      } else {
        contextBuffer = B.createAllocBox(loc,
                                         contextStorageTy.castTo<SILBoxType>(),
                                         /*debug variable*/ None,
                                         /*dynamic lifetime*/ false,
                                         /*reflection*/ true);
        contextProj = B.createProjectBox(loc, contextBuffer, 0);
      }
      
      // Transfer the formerly partially-applied arguments into the box.
      auto appliedArgs = pa->getArguments();
      for (unsigned i = 0; i < appliedArgs.size(); ++i) {
        auto arg = appliedArgs[i];
        SILValue proj = contextProj;
        if (boxFields.size() > 1) {
          proj = B.createTupleElementAddr(loc, proj, i);
        }
        auto param = partiallyAppliedParams[i];

        switch (auto conv = param.getConvention()) {
        case ParameterConvention::Direct_Owned:
        case ParameterConvention::Direct_Unowned:
        case ParameterConvention::Direct_Guaranteed:
          // Move the value into the box.
          if (caller->hasOwnership()) {
            B.createStore(loc, arg, proj, StoreOwnershipQualifier::Init);
          } else {
            B.createStore(loc, arg, proj, StoreOwnershipQualifier::Unqualified);
          }
          break;
            
        case ParameterConvention::Indirect_In_Guaranteed:
        case ParameterConvention::Indirect_In_Constant:
        case ParameterConvention::Indirect_In:
          // Move the value from its current memory location to the box.
          B.createCopyAddr(loc, arg, proj, IsTake, IsInitialization);
          break;
          
        case ParameterConvention::Indirect_InoutAliasable:
        case ParameterConvention::Indirect_Inout: {
          // Pass a pointer to the argument into the box.
          auto p = B.createAddressToPointer(loc, arg,
                                            SILType::getRawPointerType(C));
          if (caller->hasOwnership()) {
            B.createStore(loc, p, proj, StoreOwnershipQualifier::Trivial);
          } else {
            B.createStore(loc, p, proj, StoreOwnershipQualifier::Unqualified);
          }
        }
        }
      }
      
      // Partially apply the new box to create the closure.
      auto paConvention = isNoEscape ? ParameterConvention::Direct_Guaranteed
                                     : contextParam.getConvention();
      auto paOnStack = isNoEscape ? PartialApplyInst::OnStack
                                  : PartialApplyInst::NotOnStack;
      auto newPA = B.createPartialApply(loc, newFunctionRef,
                                        pa->getSubstitutionMap(),
                                        contextBuffer,
                                        paConvention,
                                        paOnStack);
      assert(isSimplePartialApply(newPA)
             && "partial apply wasn't simple after transformation?");
      pa->replaceAllUsesWith(newPA);
      pa->eraseFromParent();
    }
    
    // Once all the partial applications have been rewritten, then the original
    // function refs with the old function type should all be unused. Delete
    // them, since they are no longer valid.
    for (auto fr : pa.FunctionRefs) {
      fr->eraseFromParent();
    }

    ++NumInvocationFunctionsChanged;
    return;
  }
  // Otherwise, we'll produce forwarding thunks for the different partial
  // application shapes.
create_forwarding_thunks:
  LLVM_DEBUG(llvm::dbgs() << "TODO: create forwarding thunk here\n");
  return;
}

void PartialApplySimplificationPass::processDynamicCallee(PartialApplyInst *pa){
  // TODO
  ++NumDynamicPartialApplicationForwarders;
}

SILTransform *swift::createPartialApplySimplification() {
  return new PartialApplySimplificationPass();
}
