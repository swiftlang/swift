//===--- SILIsolationInfo.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/SILIsolationInfo.h"

#include "swift/AST/ASTWalker.h"
#include "swift/AST/Expr.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/AddressWalker.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/Test.h"
#include "swift/SILOptimizer/Utils/VariableNameUtils.h"

using namespace swift;
using namespace swift::PatternMatch;

static std::optional<ActorIsolation>
getGlobalActorInitIsolation(SILFunction *fn) {
  auto block = fn->begin();

  // Make sure our function has a single block. We should always have a single
  // block today. Return nullptr otherwise.
  if (block == fn->end() || std::next(block) != fn->end())
    return {};

  GlobalAddrInst *gai = nullptr;
  if (!match(cast<SILInstruction>(block->getTerminator()),
             m_ReturnInst(m_AddressToPointerInst(m_GlobalAddrInst(gai)))))
    return {};

  auto *globalDecl = gai->getReferencedGlobal()->getDecl();
  if (!globalDecl)
    return {};

  // See if our globalDecl is specifically guarded.
  return getActorIsolation(globalDecl);
}

class DeclRefExprAnalysis {
  DeclRefExpr *result = nullptr;

  // Be greedy with the small size so we very rarely allocate.
  SmallVector<Expr *, 8> lookThroughExprs;

public:
  bool compute(Expr *expr);

  DeclRefExpr *getResult() const {
    assert(result && "Not computed?!");
    return result;
  }

  ArrayRef<Expr *> getLookThroughExprs() const {
    assert(result && "Not computed?!");
    return lookThroughExprs;
  }

  void print(llvm::raw_ostream &os) const {
    if (!result) {
      os << "DeclRefExprAnalysis: None.";
      return;
    }

    os << "DeclRefExprAnalysis:\n";
    result->dump(os);
    os << "\n";
    if (lookThroughExprs.size()) {
      os << "LookThroughExprs:\n";
      for (auto *expr : lookThroughExprs) {
        expr->dump(os, 4);
      }
    }
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  bool hasNonisolatedUnsafe() const {
    // See if our initial member_ref_expr is actor instance isolated.
    for (auto *expr : lookThroughExprs) {
      // We can skip load expr.
      if (isa<LoadExpr>(expr))
        continue;

      if (auto *mri = dyn_cast<MemberRefExpr>(expr)) {
        if (mri->hasDecl()) {
          auto isolation = swift::getActorIsolation(mri->getDecl().getDecl());
          if (isolation.isNonisolatedUnsafe())
            return true;
        }
      }

      break;
    }

    return false;
  }
};

bool DeclRefExprAnalysis::compute(Expr *expr) {
  struct LocalWalker final : ASTWalker {
    DeclRefExprAnalysis &parentAnalysis;

    LocalWalker(DeclRefExprAnalysis &parentAnalysis)
        : parentAnalysis(parentAnalysis) {}

    PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
      assert(!parentAnalysis.result && "Shouldn't have a result yet");

      if (auto *dre = dyn_cast<DeclRefExpr>(expr)) {
        parentAnalysis.result = dre;
        return Action::Stop();
      }

      if (isa<CoerceExpr, MemberRefExpr, ImplicitConversionExpr, IdentityExpr>(
              expr)) {
        parentAnalysis.lookThroughExprs.push_back(expr);
        return Action::Continue(expr);
      }

      return Action::Stop();
    }
  };

  LocalWalker walker(*this);

  if (auto *ae = dyn_cast<AssignExpr>(expr)) {
    ae->getSrc()->walk(walker);
  } else {
    expr->walk(walker);
  }

  return result;
}

static SILIsolationInfo
inferIsolationInfoForTempAllocStack(AllocStackInst *asi) {
  // We want to search for an alloc_stack that is not from a VarDecl and that is
  // initially isolated along all paths to the same actor isolation. If they
  // differ, then we emit a we do not understand error.
  struct AddressWalkerState {
    AllocStackInst *asi = nullptr;
    SmallVector<Operand *, 8> indirectResultUses;
    llvm::SmallSetVector<SILInstruction *, 8> writes;
    Operand *sameBlockIndirectResultUses = nullptr;
  };

  struct AddressWalker final : TransitiveAddressWalker<AddressWalker> {
    AddressWalkerState &state;

    AddressWalker(AddressWalkerState &state) : state(state) {
      assert(state.asi);
    }

    bool visitUse(Operand *use) {
      // If we do not write to memory, then it is harmless.
      if (!use->getUser()->mayWriteToMemory())
        return true;

      if (auto fas = FullApplySite::isa(use->getUser())) {
        if (fas.isIndirectResultOperand(*use)) {
          // If our indirect result use is in the same block...
          auto *parentBlock = state.asi->getParent();
          if (fas.getParent() == parentBlock) {
            // If we haven't seen any indirect result use yet... just cache it
            // and return true.
            if (!state.sameBlockIndirectResultUses) {
              state.sameBlockIndirectResultUses = use;
              return true;
            }

            // If by walking from the alloc stack to the full apply site, we do
            // not see the current sameBlockIndirectResultUses, we have a new
            // newest use.
            if (llvm::none_of(
                    llvm::make_range(state.asi->getIterator(),
                                     fas->getIterator()),
                    [&](const SILInstruction &inst) {
                      return &inst ==
                             state.sameBlockIndirectResultUses->getUser();
                    })) {
              state.sameBlockIndirectResultUses = use;
            }
            return true;
          }

          // If not, just stash it into the non-same block indirect result use
          // array.
          state.indirectResultUses.push_back(use);
          return true;
        }
      }

      state.writes.insert(use->getUser());
      return true;
    }
  };

  AddressWalkerState state;
  state.asi = asi;
  AddressWalker walker(state);

  // If we fail to walk, emit an unknown patten error.
  if (AddressUseKind::Unknown == std::move(walker).walk(asi)) {
    return SILIsolationInfo();
  }

  // If we do not have any indirect result uses... we can just assign fresh.
  if (!state.sameBlockIndirectResultUses && state.indirectResultUses.empty())
    return SILIsolationInfo::getDisconnected(false /*isUnsafeNonIsolated*/);

  // Otherwise, lets see if we had a same block indirect result.
  if (state.sameBlockIndirectResultUses) {
    // Check if this indirect result has a sending result. In such a case, we
    // always return disconnected.
    if (auto fas =
            FullApplySite::isa(state.sameBlockIndirectResultUses->getUser())) {
      if (fas.getSubstCalleeType()->hasSendingResult())
        return SILIsolationInfo::getDisconnected(
            false /*is unsafe non isolated*/);
    }

    // If we do not have any writes in between the alloc stack and the
    // initializer, then we have a good target. Otherwise, we just return
    // AssignFresh.
    if (llvm::none_of(
            llvm::make_range(
                asi->getIterator(),
                state.sameBlockIndirectResultUses->getUser()->getIterator()),
            [&](SILInstruction &inst) { return state.writes.count(&inst); })) {
      auto isolationInfo =
          SILIsolationInfo::get(state.sameBlockIndirectResultUses->getUser());
      if (isolationInfo) {
        return isolationInfo;
      }
    }

    // If we did not find an isolation info, just do a normal assign fresh.
    return SILIsolationInfo::getDisconnected(false /*is unsafe non isolated*/);
  }

  // Check if any of our writes are within the first block. This would
  // automatically stop our search and we should assign fresh. Since we are
  // going over the writes here, also setup a writeBlocks set.
  auto *defBlock = asi->getParent();
  BasicBlockSet writeBlocks(defBlock->getParent());
  for (auto *write : state.writes) {
    if (write->getParent() == defBlock)
      return SILIsolationInfo::getDisconnected(false /*unsafe non isolated*/);
    writeBlocks.insert(write->getParent());
  }

  // Ok, at this point we know that we do not have any indirect result uses in
  // the def block and also we do not have any writes in that initial
  // block. This sets us up for our global analysis. Our plan is as follows:
  //
  // 1. We are going to create a set of writeBlocks and a map from SILBasicBlock
  // -> first indirect result block if there isn't a write before it.
  //
  // 2. We walk from our def block until we reach the first indirect result
  // block. We stop processing successor if we find a write block successor that
  // is not also an indirect result block. This makes sense since we earlier
  // required that any notates indirect result block do not have any writes in
  // between the indirect result and the beginning of the block.
  llvm::SmallDenseMap<SILBasicBlock *, Operand *, 2> blockToOperandMap;
  for (auto *use : state.indirectResultUses) {
    // If our indirect result use has a write before it in the block, do not
    // store it. It cannot be our indirect result initializer.
    if (writeBlocks.contains(use->getParentBlock()) &&
        llvm::any_of(
            use->getParentBlock()->getRangeEndingAtInst(use->getUser()),
            [&](SILInstruction &inst) { return state.writes.contains(&inst); }))
      continue;

    // Ok, we now know that there aren't any writes before us in the block. Now
    // try to insert.
    auto iter = blockToOperandMap.try_emplace(use->getParentBlock(), use);

    // If we actually inserted, then we are done.
    if (iter.second) {
      continue;
    }

    // Otherwise, if we are before the current value, set us to be the value
    // instead.
    if (llvm::none_of(
            use->getParentBlock()->getRangeEndingAtInst(use->getUser()),
            [&](const SILInstruction &inst) {
              return &inst == iter.first->second->getUser();
            })) {
      iter.first->getSecond() = use;
    }
  }

  // Ok, we now have our data all setup.
  BasicBlockWorklist worklist(asi->getFunction());
  for (auto *succBlock : asi->getParentBlock()->getSuccessorBlocks()) {
    worklist.pushIfNotVisited(succBlock);
  }

  Operand *targetOperand = nullptr;
  while (auto *next = worklist.pop()) {
    // First check if this is one of our target blocks.
    auto iter = blockToOperandMap.find(next);

    // If this is our target blocks...
    if (iter != blockToOperandMap.end()) {
      // If we already have an assigned target block, make sure this is the same
      // one. If it is, just continue. Otherwise, something happened we do not
      // understand... assign fresh.
      if (!targetOperand) {
        targetOperand = iter->second;
        continue;
      }

      if (targetOperand->getParentBlock() == iter->first) {
        continue;
      }

      return SILIsolationInfo::getDisconnected(
          false /*is unsafe non isolated*/);
    }

    // Otherwise, see if this block is a write block. If so, we have a path to a
    // write block that does not go through one of our blockToOperandMap
    // blocks... return assign fresh.
    if (writeBlocks.contains(next))
      return SILIsolationInfo::getDisconnected(
          false /*is unsafe non isolated*/);

    // Otherwise, visit this blocks successors if we have not yet visited them.
    for (auto *succBlock : next->getSuccessorBlocks()) {
      worklist.pushIfNotVisited(succBlock);
    }
  }

  // At this point, we know that we have a single indirect result use that
  // dominates all writes and other indirect result uses. We can say that our
  // alloc_stack temporary is that indirect result use's isolation.
  if (auto fas = FullApplySite::isa(targetOperand->getUser())) {
    if (fas.getSubstCalleeType()->hasSendingResult())
      return SILIsolationInfo::getDisconnected(
          false /*is unsafe non isolated*/);
  }
  return SILIsolationInfo::get(targetOperand->getUser());
}

SILIsolationInfo SILIsolationInfo::get(SILInstruction *inst) {
  if (auto fas = FullApplySite::isa(inst)) {
    // Check if we have SIL based "faked" isolation crossings that we use for
    // testing purposes.
    //
    // NOTE: Please do not use getWithIsolationCrossing in more places! We only
    // want to use it here!
    if (auto crossing = fas.getIsolationCrossing()) {
      if (auto info = SILIsolationInfo::getWithIsolationCrossing(*crossing))
        return info;
    }

    if (auto *isolatedOp = fas.getIsolatedArgumentOperandOrNullPtr()) {
      // First look through ActorInstance agnostic values so we can find the
      // type of the actual underlying actor (e.x.: copy_value,
      // init_existential_ref, begin_borrow, etc).
      auto actualIsolatedValue =
          ActorInstance::lookThroughInsts(isolatedOp->get());

      // First see if we have a .none enum inst. In such a case, we are actually
      // on the nonisolated global queue.
      if (auto *ei = dyn_cast<EnumInst>(actualIsolatedValue)) {
        if (ei->getElement()->getParentEnum()->isOptionalDecl() &&
            !ei->hasOperand()) {
          // In this case, we have a .none so we are attempting to use the
          // global queue. This means that the isolation effect of the
          // function is disconnected since we are treating the function as
          // nonisolated.
          return SILIsolationInfo::getDisconnected(false);
        }
      }

      // Then using that value, grab the AST type from the actual isolated
      // value.
      CanType selfASTType = actualIsolatedValue->getType().getASTType();

      // Then look through optional types since in cases like where we have a
      // function argument that is an Optional actor... like an optional actor
      // returned from a function, we still need to be able to lookup the actor
      // as being the underlying type.
      selfASTType =
          selfASTType->lookThroughAllOptionalTypes()->getCanonicalType();
      if (auto *nomDecl = selfASTType->getAnyActor()) {
        // Then see if we have a global actor. This pattern matches the output
        // for doing things like GlobalActor.shared.
        if (nomDecl->isGlobalActor()) {
          return SILIsolationInfo::getGlobalActorIsolated(SILValue(), nomDecl);
        }

        // TODO: We really should be doing this based off of an Operand. Then
        // we would get the SILValue() for the first element. Today this can
        // only mess up isolation history.
        return SILIsolationInfo::getActorInstanceIsolated(
            SILValue(), actualIsolatedValue, nomDecl);
      }
    }

    // See if we can infer isolation from our callee.
    if (auto isolationInfo = get(fas.getCallee())) {
      return isolationInfo;
    }
  }

  if (auto *pai = dyn_cast<PartialApplyInst>(inst)) {
    if (auto *ace = pai->getLoc().getAsASTNode<AbstractClosureExpr>()) {
      auto actorIsolation = ace->getActorIsolation();

      if (actorIsolation.isGlobalActor()) {
        return SILIsolationInfo::getGlobalActorIsolated(
            pai, actorIsolation.getGlobalActor());
      }

      if (actorIsolation.isActorInstanceIsolated()) {
        ApplySite as(pai);
        SILValue actorInstance;
        for (auto &op : as.getArgumentOperands()) {
          if (as.getArgumentParameterInfo(op).hasOption(
                  SILParameterInfo::Isolated)) {
            actorInstance = op.get();
            break;
          }
        }

        if (actorInstance) {
          return SILIsolationInfo::getActorInstanceIsolated(
              pai, actorInstance, actorIsolation.getActor());
        }

        // For now, if we do not have an actor instance, just create an actor
        // instance isolated without an actor instance.
        //
        // If we do not have an actor instance, that means that we have a
        // partial apply for which the isolated parameter was not closed over
        // and is an actual argument that we pass in. This means that the
        // partial apply is actually flow sensitive in terms of which specific
        // actor instance we are isolated to.
        //
        // TODO: How do we want to resolve this.
        return SILIsolationInfo::getPartialApplyActorInstanceIsolated(
            pai, actorIsolation.getActor());
      }

      assert(actorIsolation.getKind() != ActorIsolation::Erased &&
             "Implement this!");
    }
  }

  // See if the memory base is a ref_element_addr from an address. If so, add
  // the actor derived flag.
  //
  // This is important so we properly handle setters.
  if (auto *rei = dyn_cast<RefElementAddrInst>(inst)) {
    auto varIsolation = swift::getActorIsolation(rei->getField());

    auto *nomDecl =
        rei->getOperand()->getType().getNominalOrBoundGenericNominal();

    if (nomDecl->isAnyActor())
      return SILIsolationInfo::getActorInstanceIsolated(rei, rei->getOperand(),
                                                        nomDecl)
          .withUnsafeNonIsolated(varIsolation.isNonisolatedUnsafe());

    if (auto isolation = swift::getActorIsolation(nomDecl)) {
      assert(isolation.isGlobalActor());
      return SILIsolationInfo::getGlobalActorIsolated(
                 rei, isolation.getGlobalActor())
          .withUnsafeNonIsolated(varIsolation.isNonisolatedUnsafe());
    }

    return SILIsolationInfo::getDisconnected(
        varIsolation.isNonisolatedUnsafe());
  }

  // Check if we have a global_addr inst.
  if (auto *ga = dyn_cast<GlobalAddrInst>(inst)) {
    if (auto *global = ga->getReferencedGlobal()) {
      if (auto *globalDecl = global->getDecl()) {
        auto isolation = swift::getActorIsolation(globalDecl);
        if (isolation.isGlobalActor()) {
          return SILIsolationInfo::getGlobalActorIsolated(
              ga, isolation.getGlobalActor());
        }

        if (isolation.isNonisolatedUnsafe()) {
          return SILIsolationInfo::getDisconnected(
              true /*is nonisolated(unsafe)*/);
        }
      }
    }
  }

  // Treat function ref as either actor isolated or sendable.
  if (auto *fri = dyn_cast<FunctionRefInst>(inst)) {
    if (auto optIsolation = fri->getReferencedFunction()->getActorIsolation()) {
      auto isolation = *optIsolation;

      // First check if we are actor isolated at the AST level... if we are,
      // then create the relevant actor isolated.
      if (isolation.isActorIsolated()) {
        if (isolation.isGlobalActor()) {
          return SILIsolationInfo::getGlobalActorIsolated(
              fri, isolation.getGlobalActor());
        }

        // TODO: We need to be able to support flow sensitive actor instances
        // like we do for partial apply. Until we do so, just store SILValue()
        // for this. This could cause a problem if we can construct a function
        // ref and invoke it with two different actor instances of the same type
        // and pass in the same parameters to both. We should error and we would
        // not with this impl since we could not distinguish the two.
        if (isolation.getKind() == ActorIsolation::ActorInstance) {
          return SILIsolationInfo::getFlowSensitiveActorIsolated(fri,
                                                                 isolation);
        }

        assert(isolation.getKind() != ActorIsolation::Erased &&
               "Implement this!");
      }

      // Then check if we have something that is nonisolated unsafe.
      if (isolation.isNonisolatedUnsafe()) {
        // First check if our function_ref is a method of a global actor
        // isolated type. In such a case, we create a global actor isolated
        // nonisolated(unsafe) so that if we assign the value to another
        // variable, the variable still says that it is the appropriate global
        // actor isolated thing.
        //
        // E.x.:
        //
        // @MainActor
        // struct X { nonisolated(unsafe) var x: NonSendableThing { ... } }
        //
        // We want X.x to be safe to use... but to have that 'z' in the
        // following is considered MainActor isolated.
        //
        // let z = X.x
        //
        auto *func = fri->getReferencedFunction();
        auto funcType = func->getLoweredFunctionType();
        if (funcType->hasSelfParam()) {
          auto selfParam = funcType->getSelfInstanceType(
              fri->getModule(), func->getTypeExpansionContext());
          if (auto *nomDecl = selfParam->getNominalOrBoundGenericNominal()) {
            auto nomDeclIsolation = swift::getActorIsolation(nomDecl);
            if (nomDeclIsolation.isGlobalActor()) {
              return SILIsolationInfo::getGlobalActorIsolated(
                         fri, nomDeclIsolation.getGlobalActor())
                  .withUnsafeNonIsolated(true);
            }
          }
        }
      }
    }

    // Otherwise, lets look at the AST and see if our function ref is from an
    // autoclosure.
    if (auto *autoclosure = fri->getLoc().getAsASTNode<AutoClosureExpr>()) {
      if (auto *funcType = autoclosure->getType()->getAs<AnyFunctionType>()) {
        if (funcType->hasGlobalActor()) {
          if (funcType->hasGlobalActor()) {
            return SILIsolationInfo::getGlobalActorIsolated(
                fri, funcType->getGlobalActor());
          }
        }

        if (auto *resultFType =
                funcType->getResult()->getAs<AnyFunctionType>()) {
          if (resultFType->hasGlobalActor()) {
            return SILIsolationInfo::getGlobalActorIsolated(
                fri, resultFType->getGlobalActor());
          }
        }
      }
    }
  }

  if (auto *cmi = dyn_cast<ClassMethodInst>(inst)) {
    // Ok, we know that we do not have an actor... but we might have a global
    // actor isolated method. Use the AST to compute the actor isolation and
    // check if we are self. If we are not self, we want this to be
    // disconnected.
    if (auto *expr = cmi->getLoc().getAsASTNode<Expr>()) {
      DeclRefExprAnalysis exprAnalysis;
      if (exprAnalysis.compute(expr)) {
        auto *dre = exprAnalysis.getResult();

        // First see if we can get any information from the actual var decl of
        // the class_method. We could find isolation or if our value is marked
        // as nonisolated(unsafe), we could find that as well. If we have
        // nonisolated(unsafe), we just propagate the value. Otherwise, we
        // return the isolation.
        bool isNonIsolatedUnsafe = exprAnalysis.hasNonisolatedUnsafe();
        {
          auto isolation = swift::getActorIsolation(dre->getDecl());

          if (isolation.isActorIsolated()) {
            // Check if we have a global actor and handle it appropriately.
            if (isolation.getKind() == ActorIsolation::GlobalActor) {
              bool localNonIsolatedUnsafe =
                  isNonIsolatedUnsafe | isolation.isNonisolatedUnsafe();
              return SILIsolationInfo::getGlobalActorIsolated(
                         cmi, isolation.getGlobalActor())
                  .withUnsafeNonIsolated(localNonIsolatedUnsafe);
            }

            // In this case, we have an actor instance that is self.
            if (isolation.getKind() != ActorIsolation::ActorInstance &&
                isolation.isActorInstanceForSelfParameter()) {
              bool localNonIsolatedUnsafe =
                  isNonIsolatedUnsafe | isolation.isNonisolatedUnsafe();
              return SILIsolationInfo::getActorInstanceIsolated(
                         cmi, cmi->getOperand(),
                         cmi->getOperand()
                             ->getType()
                             .getNominalOrBoundGenericNominal())
                  .withUnsafeNonIsolated(localNonIsolatedUnsafe);
            }
          }
        }

        if (auto type = dre->getType()->getNominalOrBoundGenericNominal()) {
          if (auto isolation = swift::getActorIsolation(type)) {
            if (isolation.isActorIsolated()) {
              // Check if we have a global actor and handle it appropriately.
              if (isolation.getKind() == ActorIsolation::GlobalActor) {
                bool localNonIsolatedUnsafe =
                    isNonIsolatedUnsafe | isolation.isNonisolatedUnsafe();
                return SILIsolationInfo::getGlobalActorIsolated(
                           cmi, isolation.getGlobalActor())
                    .withUnsafeNonIsolated(localNonIsolatedUnsafe);
              }

              // In this case, we have an actor instance that is self.
              if (isolation.getKind() != ActorIsolation::ActorInstance &&
                  isolation.isActorInstanceForSelfParameter()) {
                bool localNonIsolatedUnsafe =
                    isNonIsolatedUnsafe | isolation.isNonisolatedUnsafe();
                return SILIsolationInfo::getActorInstanceIsolated(
                           cmi, cmi->getOperand(),
                           cmi->getOperand()
                               ->getType()
                               .getNominalOrBoundGenericNominal())
                    .withUnsafeNonIsolated(localNonIsolatedUnsafe);
              }
            }
          }
        }

        if (isNonIsolatedUnsafe)
          return SILIsolationInfo::getDisconnected(isNonIsolatedUnsafe);
      }
    }
  }

  // See if we have a struct_extract from a global actor isolated type.
  if (auto *sei = dyn_cast<StructExtractInst>(inst)) {
    auto varIsolation = swift::getActorIsolation(sei->getField());
    if (auto isolation =
            SILIsolationInfo::getGlobalActorIsolated(sei, sei->getStructDecl()))
      return isolation.withUnsafeNonIsolated(
          varIsolation.isNonisolatedUnsafe());
    return SILIsolationInfo::getDisconnected(
        varIsolation.isNonisolatedUnsafe());
  }

  if (auto *seai = dyn_cast<StructElementAddrInst>(inst)) {
    auto varIsolation = swift::getActorIsolation(seai->getField());
    if (auto isolation = SILIsolationInfo::getGlobalActorIsolated(
            seai, seai->getStructDecl()))
      return isolation.withUnsafeNonIsolated(
          varIsolation.isNonisolatedUnsafe());
    return SILIsolationInfo::getDisconnected(
        varIsolation.isNonisolatedUnsafe());
  }

  // See if we have an unchecked_enum_data from a global actor isolated type.
  if (auto *uedi = dyn_cast<UncheckedEnumDataInst>(inst)) {
    return SILIsolationInfo::getGlobalActorIsolated(uedi, uedi->getEnumDecl());
  }

  // See if we have an unchecked_enum_data from a global actor isolated type.
  if (auto *utedi = dyn_cast<UncheckedTakeEnumDataAddrInst>(inst)) {
    return SILIsolationInfo::getGlobalActorIsolated(utedi,
                                                    utedi->getEnumDecl());
  }

  // Check if we have an unsafeMutableAddressor from a global actor, mark the
  // returned value as being actor derived.
  if (auto applySite = dyn_cast<ApplyInst>(inst)) {
    if (auto *calleeFunction = applySite->getCalleeFunction()) {
      if (calleeFunction->isGlobalInit()) {
        auto isolation = getGlobalActorInitIsolation(calleeFunction);
        if (isolation && isolation->isGlobalActor()) {
          return SILIsolationInfo::getGlobalActorIsolated(
              applySite, isolation->getGlobalActor());
        }
      }
    }
  }

  // See if we have a convert function from a Sendable actor isolated function,
  // we want to treat the result of the convert function as being actor isolated
  // so that we cannot escape the value.
  //
  // NOTE: At this point, we already know that cfi's result is not sendable,
  // since we would have exited above already.
  if (auto *cfi = dyn_cast<ConvertFunctionInst>(inst)) {
    SILValue operand = cfi->getOperand();
    if (operand->getType().getAs<SILFunctionType>()->isSendable()) {
      SILValue newValue = operand;
      do {
        operand = newValue;

        newValue = lookThroughOwnershipInsts(operand);
        if (auto *ttfi = dyn_cast<ThinToThickFunctionInst>(newValue)) {
          newValue = ttfi->getOperand();
        }

        if (auto *cfi = dyn_cast<ConvertFunctionInst>(newValue)) {
          newValue = cfi->getOperand();
        }

        if (auto *pai = dyn_cast<PartialApplyInst>(newValue)) {
          newValue = pai->getCallee();
        }
      } while (newValue != operand);

      if (auto *ai = dyn_cast<ApplyInst>(operand)) {
        if (auto *callExpr = ai->getLoc().getAsASTNode<ApplyExpr>()) {
          if (auto *callType = callExpr->getType()->getAs<AnyFunctionType>()) {
            if (callType->hasGlobalActor()) {
              return SILIsolationInfo::getGlobalActorIsolated(
                  ai, callType->getGlobalActor());
            }
          }
        }
      }

      if (auto *fri = dyn_cast<FunctionRefInst>(operand)) {
        if (auto isolation = SILIsolationInfo::get(fri)) {
          return isolation;
        }
      }
    }
  }

  if (auto *asi = dyn_cast<AllocStackInst>(inst)) {
    if (asi->isFromVarDecl()) {
      if (auto *varDecl = asi->getLoc().getAsASTNode<VarDecl>()) {
        auto isolation = swift::getActorIsolation(varDecl);
        if (isolation.getKind() == ActorIsolation::NonisolatedUnsafe) {
          return SILIsolationInfo::getDisconnected(
              true /*is nonisolated(unsafe)*/);
        }
      }
    } else {
      // Ok, we have a temporary. If it is non-Sendable...
      if (SILIsolationInfo::isNonSendableType(asi)) {
        if (auto isolation = inferIsolationInfoForTempAllocStack(asi))
          return isolation;
      }
    }
  }

  if (auto *mvi = dyn_cast<MoveValueInst>(inst)) {
    if (mvi->isFromVarDecl()) {
      if (auto *debugInfo = getSingleDebugUse(mvi)) {
        if (auto *dbg = dyn_cast<DebugValueInst>(debugInfo->getUser())) {
          if (auto *varDecl = dbg->getLoc().getAsASTNode<VarDecl>()) {
            auto isolation = swift::getActorIsolation(varDecl);
            if (isolation.getKind() == ActorIsolation::NonisolatedUnsafe) {
              return SILIsolationInfo::getDisconnected(
                  true /*is nonisolated(unsafe)*/);
            }
          }
        }
      }
    }
  }

  // Check if we have an ApplyInst with nonisolated.
  //
  // NOTE: We purposely avoid using other isolation info from an ApplyExpr since
  // when we use the isolation crossing on the ApplyExpr at this point,w e are
  // unable to find out what the appropriate instance is (since we would have
  // found it earlier if we could). This ensures that we can eliminate a case
  // where we get a SILIsolationInfo with actor isolation and without a SILValue
  // actor instance. This prevents a class of bad SILIsolationInfo merge errors
  // caused by the actor instances not matching.
  if (ApplyExpr *apply = inst->getLoc().getAsASTNode<ApplyExpr>()) {
    if (auto crossing = apply->getIsolationCrossing()) {
      if (crossing->getCalleeIsolation().isNonisolated()) {
        return SILIsolationInfo::getDisconnected(false /*nonisolated(unsafe)*/);
      }
    }
  }

  return SILIsolationInfo();
}

SILIsolationInfo SILIsolationInfo::get(SILArgument *arg) {
  // Return early if we do not have a non-Sendable type.
  if (!SILIsolationInfo::isNonSendableType(arg->getType(), arg->getFunction()))
    return {};

  // Handle a switch_enum from a global actor isolated type.
  if (auto *phiArg = dyn_cast<SILPhiArgument>(arg)) {
    if (auto *singleTerm = phiArg->getSingleTerminator()) {
      if (auto *swi = dyn_cast<SwitchEnumInst>(singleTerm)) {
        auto enumDecl =
            swi->getOperand()->getType().getEnumOrBoundGenericEnum();
        return SILIsolationInfo::getGlobalActorIsolated(arg, enumDecl);
      }
    }
    return SILIsolationInfo();
  }

  auto *fArg = cast<SILFunctionArgument>(arg);

  // Sending is always disconnected.
  if (fArg->isSending())
    return SILIsolationInfo::getDisconnected(false /*nonisolated(unsafe)*/);

  // If we have a closure capture that is not an indirect result or indirect
  // result error, we want to treat it as sending so that we properly handle
  // async lets.
  //
  // This pattern should only come up with async lets. See comment in
  // isTransferrableFunctionArgument.
  if (!fArg->isIndirectResult() && !fArg->isIndirectErrorResult() &&
      fArg->isClosureCapture() &&
      fArg->getFunction()->getLoweredFunctionType()->isSendable())
    return SILIsolationInfo::getDisconnected(false /*nonisolated(unsafe)*/);

  // Before we do anything further, see if we have an isolated parameter. This
  // handles isolated self and specifically marked isolated.
  if (auto *isolatedArg = fArg->getFunction()->maybeGetIsolatedArgument()) {
    auto astType = isolatedArg->getType().getASTType();
    if (auto *nomDecl = astType->lookThroughAllOptionalTypes()->getAnyActor()) {
      return SILIsolationInfo::getActorInstanceIsolated(fArg, isolatedArg,
                                                        nomDecl);
    }
  }

  // Otherwise, see if we need to handle this isolation computation specially
  // due to information from the decl ref if we have one.
  if (auto declRef = fArg->getFunction()->getDeclRef()) {
    // First check if we have an allocator decl ref. If we do and we have an
    // actor instance isolation, then we know that we are actively just calling
    // the initializer. To just make region isolation work, treat this as
    // disconnected so we can construct the actor value. Users cannot write
    // allocator functions so we just need to worry about compiler generated
    // code. In the case of a non-actor, we can only have an allocator that is
    // global actor isolated, so we will never hit this code path.
    if (declRef.kind == SILDeclRef::Kind::Allocator) {
      if (auto isolation = fArg->getFunction()->getActorIsolation()) {
        if (isolation->isActorInstanceIsolated()) {
          return SILIsolationInfo::getDisconnected(
              false /*nonisolated(unsafe)*/);
        }
      }
    }

    // Then see if we have an init accessor that is isolated to an actor
    // instance, but for which we have not actually passed self. In such a case,
    // we need to pass in a "fake" ActorInstance that users know is a sentinel
    // for the self value.
    if (auto functionIsolation = fArg->getFunction()->getActorIsolation()) {
      if (functionIsolation->isActorInstanceIsolated() && declRef.getDecl()) {
        if (auto *accessor =
                dyn_cast_or_null<AccessorDecl>(declRef.getFuncDecl())) {
          if (accessor->isInitAccessor()) {
            return SILIsolationInfo::getActorInstanceIsolated(
                fArg, ActorInstance::getForActorAccessorInit(),
                functionIsolation->getActor());
          }
        }
      }
    }
  }

  // Otherwise, if we do not have an isolated argument and are not in an
  // allocator, then we might be isolated via global isolation.
  if (auto functionIsolation = fArg->getFunction()->getActorIsolation()) {
    if (functionIsolation->isActorIsolated()) {
      if (functionIsolation->isGlobalActor()) {
        return SILIsolationInfo::getGlobalActorIsolated(
            fArg, functionIsolation->getGlobalActor());
      }

      return SILIsolationInfo::getActorInstanceIsolated(
          fArg, ActorInstance::getForActorAccessorInit(),
          functionIsolation->getActor());
    }
  }

  return SILIsolationInfo::getTaskIsolated(fArg);
}

void SILIsolationInfo::printOptions(llvm::raw_ostream &os) const {
  auto opts = getOptions();
  if (!opts)
    return;

  os << ": ";

  llvm::SmallVector<StringLiteral, unsigned(Flag::MaxNumBits)> data;

  if (opts.contains(Flag::UnsafeNonIsolated)) {
    data.push_back(StringLiteral("nonisolated(unsafe)"));
    opts -= Flag::UnsafeNonIsolated;
  }

  if (opts.contains(Flag::UnappliedIsolatedAnyParameter)) {
    data.push_back(StringLiteral("unapplied_isolated_any_parameter"));
    opts -= Flag::UnappliedIsolatedAnyParameter;
  }

  assert(!opts && "Unhandled flag?!");
  assert(data.size() < unsigned(Flag::MaxNumBits) &&
         "Please update MaxNumBits so that we can avoid heap allocations in "
         "this SmallVector");

  llvm::interleave(data, os, ", ");
}

void SILIsolationInfo::print(llvm::raw_ostream &os) const {
  switch (Kind(*this)) {
  case Unknown:
    os << "unknown";
    return;
  case Disconnected:
    os << "disconnected";
    printOptions(os);
    return;
  case Actor:
    if (ActorInstance instance = getActorInstance()) {
      switch (instance.getKind()) {
      case ActorInstance::Kind::Value: {
        SILValue value = instance.getValue();
        if (auto name = VariableNameInferrer::inferName(value)) {
          os << "'" << *name << "'-isolated";
          printOptions(os);
          os << "\n";
          os << "instance: " << *value;

          return;
        }
        break;
      }
      case ActorInstance::Kind::ActorAccessorInit:
        os << "'self'-isolated";
        printOptions(os);
        os << '\n';
        os << "instance: actor accessor init\n";
        return;
      case ActorInstance::Kind::CapturedActorSelf:
        os << "'self'-isolated";
        printOptions(os);
        os << '\n';
        os << "instance: captured actor instance self\n";
        return;
      }
    }

    if (getActorIsolation().getKind() == ActorIsolation::ActorInstance) {
      if (auto *vd = getActorIsolation().getActorInstance()) {
        os << "'" << vd->getBaseIdentifier() << "'-isolated";
        printOptions(os);
        return;
      }
    }

    getActorIsolation().printForDiagnostics(os);
    printOptions(os);
    return;
  case Task:
    os << "task-isolated";
    printOptions(os);
    os << '\n';
    os << "instance: " << *getIsolatedValue();
    return;
  }
}

bool SILIsolationInfo::hasSameIsolation(ActorIsolation actorIsolation) const {
  if (getKind() != Kind::Actor)
    return false;
  return getActorIsolation() == actorIsolation;
}

bool SILIsolationInfo::hasSameIsolation(const SILIsolationInfo &other) const {
  if (getKind() != other.getKind())
    return false;

  switch (getKind()) {
  case Unknown:
  case Disconnected:
    return true;
  case Task:
    return getIsolatedValue() == other.getIsolatedValue();
  case Actor: {
    ActorInstance actor1 = getActorInstance();
    ActorInstance actor2 = other.getActorInstance();

    // If either have an actor instance, and the actor instance doesn't match,
    // return false.
    //
    // This ensures that cases like comparing two global actor isolated things
    // do not hit this path.
    //
    // It also catches cases where we have a missing actor instance.
    if ((actor1 || actor2) && actor1 != actor2)
      return false;

    auto lhsIsolation = getActorIsolation();
    auto rhsIsolation = other.getActorIsolation();
    return lhsIsolation == rhsIsolation;
  }
  }
}

bool SILIsolationInfo::isEqual(const SILIsolationInfo &other) const {
  // First check if the two types have the same isolation.
  if (!hasSameIsolation(other))
    return false;

  // Then check if both have the same isolated value state. If they do not
  // match, bail they cannot equal.
  if (hasIsolatedValue() != other.hasIsolatedValue())
    return false;

  // Then actually check if we have an isolated value. If we do not, then both
  // do not have an isolated value due to our earlier check, so we can just
  // return true early.
  if (!hasIsolatedValue())
    return true;

  // Otherwise, equality is determined by directly comparing the isolated value.
  return getIsolatedValue() == other.getIsolatedValue();
}

void SILIsolationInfo::Profile(llvm::FoldingSetNodeID &id) const {
  id.AddInteger(getKind());
  switch (getKind()) {
  case Unknown:
  case Disconnected:
    return;
  case Task:
    id.AddPointer(getIsolatedValue());
    return;
  case Actor:
    id.AddPointer(getIsolatedValue());
    getActorIsolation().Profile(id);
    return;
  }
}

void SILIsolationInfo::printForDiagnostics(llvm::raw_ostream &os) const {
  switch (Kind(*this)) {
  case Unknown:
    llvm::report_fatal_error("Printing unknown for diagnostics?!");
    return;
  case Disconnected:
    os << "disconnected";
    return;
  case Actor:
    if (auto instance = getActorInstance()) {
      switch (instance.getKind()) {
      case ActorInstance::Kind::Value: {
        SILValue value = instance.getValue();
        if (auto name = VariableNameInferrer::inferName(value)) {
          os << "'" << *name << "'-isolated";
          return;
        }
        break;
      }
      case ActorInstance::Kind::ActorAccessorInit:
        os << "'self'-isolated";
        return;
      case ActorInstance::Kind::CapturedActorSelf:
        os << "'self'-isolated";
        return;
      }
    }

    if (getActorIsolation().getKind() == ActorIsolation::ActorInstance) {
      if (auto *vd = getActorIsolation().getActorInstance()) {
        os << "'" << vd->getBaseIdentifier() << "'-isolated";
        return;
      }
    }

    getActorIsolation().printForDiagnostics(os);
    return;
  case Task:
    os << "task-isolated";
    return;
  }
}

void SILIsolationInfo::printForCodeDiagnostic(llvm::raw_ostream &os) const {
  switch (Kind(*this)) {
  case Unknown:
    llvm::report_fatal_error("Printing unknown for code diagnostic?!");
    return;
  case Disconnected:
    llvm::report_fatal_error("Printing disconnected for code diagnostic?!");
    return;
  case Actor:
    if (auto instance = getActorInstance()) {
      switch (instance.getKind()) {
      case ActorInstance::Kind::Value: {
        SILValue value = instance.getValue();
        if (auto name = VariableNameInferrer::inferName(value)) {
          os << "'" << *name << "'-isolated code";
          return;
        }
        break;
      }
      case ActorInstance::Kind::ActorAccessorInit:
        os << "'self'-isolated code";
        return;
      case ActorInstance::Kind::CapturedActorSelf:
        os << "'self'-isolated code";
        return;
      }
    }

    if (getActorIsolation().getKind() == ActorIsolation::ActorInstance) {
      if (auto *vd = getActorIsolation().getActorInstance()) {
        os << "'" << vd->getBaseIdentifier() << "'-isolated code";
        return;
      }
    }

    getActorIsolation().printForDiagnostics(os);
    os << " code";
    return;
  case Task:
    os << "code in the current task";
    return;
  }
}

void SILIsolationInfo::printForOneLineLogging(llvm::raw_ostream &os) const {
  switch (Kind(*this)) {
  case Unknown:
    os << "unknown";
    return;
  case Disconnected:
    os << "disconnected";
    printOptions(os);
    return;
  case Actor:
    if (auto instance = getActorInstance()) {
      switch (instance.getKind()) {
      case ActorInstance::Kind::Value: {
        SILValue value = instance.getValue();
        if (auto name = VariableNameInferrer::inferName(value)) {
          os << "'" << *name << "'-isolated";
          printOptions(os);
          return;
        }
        break;
      }
      case ActorInstance::Kind::ActorAccessorInit:
        os << "'self'-isolated (actor-accessor-init)";
        printOptions(os);
        return;
      case ActorInstance::Kind::CapturedActorSelf:
        os << "'self'-isolated (captured-actor-self)";
        printOptions(os);
        return;
      }
    }

    if (getActorIsolation().getKind() == ActorIsolation::ActorInstance) {
      if (auto *vd = getActorIsolation().getActorInstance()) {
        os << "'" << vd->getBaseIdentifier() << "'-isolated";
        printOptions(os);
        return;
      }
    }

    getActorIsolation().printForDiagnostics(os);
    printOptions(os);
    return;
  case Task:
    os << "task-isolated";
    printOptions(os);
    return;
  }
}

// Check if the passed in type is NonSendable.
//
// NOTE: We special case RawPointer and NativeObject to ensure they are
// treated as non-Sendable and strict checking is applied to it.
bool SILIsolationInfo::isNonSendableType(SILType type, SILFunction *fn) {
  // Treat Builtin.NativeObject, Builtin.RawPointer, and Builtin.BridgeObject as
  // non-Sendable.
  if (type.getASTType()->is<BuiltinNativeObjectType>() ||
      type.getASTType()->is<BuiltinRawPointerType>() ||
      type.getASTType()->is<BuiltinBridgeObjectType>()) {
    return true;
  }

  // Treat Builtin.SILToken as Sendable. It cannot escape from the current
  // function. We should change isSendable to hardwire this.
  if (type.getASTType()->is<SILTokenType>()) {
    return false;
  }

  // First before we do anything, see if we have a Sendable type. In such a
  // case, just return true early.
  //
  // DISCUSSION: It is important that we do this first since otherwise calling
  // getConcurrencyDiagnosticBehavior could cause us to prevent a
  // "preconcurrency" unneeded diagnostic when just using Sendable values. We
  // only want to trigger that if we analyze a non-Sendable type.
  if (type.isSendable(fn))
    return false;

  // Grab out behavior. If it is none, then we have a type that we want to treat
  // as non-Sendable.
  auto behavior = type.getConcurrencyDiagnosticBehavior(fn);
  if (!behavior)
    return true;

  // Finally, if we are not supposed to ignore, then we have a true non-Sendable
  // type. Types whose diagnostics we are supposed to ignore, we want to treat
  // as Sendable.
  return *behavior != DiagnosticBehavior::Ignore;
}

//===----------------------------------------------------------------------===//
//                            MARK: ActorInstance
//===----------------------------------------------------------------------===//

SILValue ActorInstance::lookThroughInsts(SILValue value) {
  if (!value)
    return value;

  while (auto *svi = dyn_cast<SingleValueInstruction>(value)) {
    if (isa<EndInitLetRefInst>(svi) || isa<CopyValueInst>(svi) ||
        isa<MoveValueInst>(svi) || isa<ExplicitCopyValueInst>(svi) ||
        isa<BeginBorrowInst>(svi) ||
        isa<CopyableToMoveOnlyWrapperValueInst>(svi) ||
        isa<MoveOnlyWrapperToCopyableValueInst>(svi) ||
        isa<InitExistentialRefInst>(svi) || isa<UncheckedRefCastInst>(svi) ||
        isa<UnconditionalCheckedCastInst>(svi)) {
      value = lookThroughInsts(svi->getOperand(0));
      continue;
    }

    // Look through extracting from optionals.
    if (auto *uedi = dyn_cast<UncheckedEnumDataInst>(svi)) {
      if (uedi->getEnumDecl() ==
          uedi->getFunction()->getASTContext().getOptionalDecl()) {
        value = lookThroughInsts(uedi->getOperand());
        continue;
      }
    }

    // Look through wrapping in an optional.
    if (auto *ei = dyn_cast<EnumInst>(svi)) {
      if (ei->hasOperand()) {
        if (ei->getElement()->getParentEnum() ==
            ei->getFunction()->getASTContext().getOptionalDecl()) {
          value = lookThroughInsts(ei->getOperand());
          continue;
        }
      }
    }

    break;
  }

  return value;
}

//===----------------------------------------------------------------------===//
//                    MARK: SILDynamicMergedIsolationInfo
//===----------------------------------------------------------------------===//

std::optional<SILDynamicMergedIsolationInfo>
SILDynamicMergedIsolationInfo::merge(SILIsolationInfo other) const {
  // If we are greater than the other kind, then we are further along the
  // lattice. We ignore the change.
  if (unsigned(innerInfo.getKind() > unsigned(other.getKind())))
    return {*this};

  // If we are both actor isolated...
  if (innerInfo.isActorIsolated() && other.isActorIsolated()) {
    // If both innerInfo and other have the same isolation, we are obviously
    // done. Just return innerInfo since we could return either.
    if (innerInfo.hasSameIsolation(other))
      return {innerInfo};

    // Ok, there is some difference in between innerInfo and other. Lets see if
    // they are both actor instance isolated and if either are unapplied
    // isolated any parameter. In such a case, take the one that is further
    // along.
    if (innerInfo.getActorIsolation().isActorInstanceIsolated() &&
        other.getActorIsolation().isActorInstanceIsolated()) {
      if (innerInfo.isUnappliedIsolatedAnyParameter())
        return other;
      if (other.isUnappliedIsolatedAnyParameter())
        return innerInfo;
    }

    // Otherwise, they do not match... so return None to signal merge failure.
    return {};
  }

  // If we are both disconnected and other has the unsafeNonIsolated bit set,
  // drop that bit and return that.
  //
  // DISCUSSION: We do not want to preserve the unsafe non isolated bit after
  // merging. These bits should not propagate through merging and should instead
  // always be associated with non-merged infos.
  if (other.isDisconnected() && other.isUnsafeNonIsolated()) {
    return {other.withUnsafeNonIsolated(false)};
  }

  // Otherwise, just return other.
  return {other};
}

//===----------------------------------------------------------------------===//
//                                MARK: Tests
//===----------------------------------------------------------------------===//

namespace swift::test {

// Arguments:
// - SILValue: value to look up isolation for.
// Dumps:
// - The inferred isolation.
static FunctionTest
    IsolationInfoInferrence("sil_isolation_info_inference",
                            [](auto &function, auto &arguments, auto &test) {
                              auto value = arguments.takeValue();

                              SILIsolationInfo info =
                                  SILIsolationInfo::get(value);
                              llvm::outs() << "Input Value: " << *value;
                              llvm::outs() << "Isolation: ";
                              info.printForOneLineLogging(llvm::outs());
                              llvm::outs() << "\n";
                            });

// Arguments:
// - SILValue: first value to merge
// - SILValue: second value to merge
// Dumps:
// - The merged isolation.
static FunctionTest IsolationMergeTest(
    "sil-isolation-info-merged-inference",
    [](auto &function, auto &arguments, auto &test) {
      auto firstValue = arguments.takeValue();
      auto secondValue = arguments.takeValue();
      SILIsolationInfo firstValueInfo = SILIsolationInfo::get(firstValue);
      SILIsolationInfo secondValueInfo = SILIsolationInfo::get(secondValue);
      std::optional<SILDynamicMergedIsolationInfo> mergedInfo(firstValueInfo);
      mergedInfo = mergedInfo->merge(secondValueInfo);
      llvm::outs() << "First Value: " << *firstValue;
      llvm::outs() << "First Isolation: ";
      firstValueInfo.printForOneLineLogging(llvm::outs());
      llvm::outs() << "\nSecond Value: " << *secondValue;
      llvm::outs() << "Second Isolation: ";
      secondValueInfo.printForOneLineLogging(llvm::outs());
      llvm::outs() << "\nMerged Isolation: ";
      if (mergedInfo) {
        mergedInfo->printForOneLineLogging(llvm::outs());
      } else {
        llvm::outs() << "Merge failure!";
      }
      llvm::outs() << "\n";
    });

} // namespace swift::test
