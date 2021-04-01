//===------- LowerHopToExecutor.cpp - Lower hop_to_executor on actors -----===//
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

#define DEBUG_TYPE "insert-hop-to-executor"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/Dominance.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/ScopedHashTable.h"

using namespace swift;

namespace {

/// Lower hop_to_executor instructions with actor operands.
///
/// While the language centers actors as the core concept, the runtime
/// is largely expressed in terms of executors, which intentionally are
/// an independent concept.  Every actor has an executor; actors can
/// customize their executor, subject to three restrictions:
///
/// - Any given actor must report the same executor every time its
///   executor is derived.  An actor can be lazy about creating its
///   executor, but it can't have multiple executors, even at different
///   points in its lifetime.
///
/// - Keeping the actor reference alive must keep the executor alive.
///
/// - Derivations of the executor may be freely removed, combined,
///   or sunk by the compiler.  (Whether they should also be hoistable
///   is more contentious.)
///
/// To facilitate full optimization of hops, SILGen emits hops to actors
/// with a hop_to_executor with an actor operand.  (Among other benefits,
/// this means that OptimizeHopToExecutor will eliminate the derivation
/// operation associated with the hop.)  This pass then comes along and
/// inserts the derivations, turning those hops into hops to executors.
/// IRGen expects hops to be to executors before it runs.
class LowerHopToActor {
  SILFunction *F;
  SILBuilder B;
  DominanceInfo *Dominance;

  /// A map from an actor value to the executor we've derived for it.
  llvm::ScopedHashTable<SILValue, SILValue> ExecutorForActor;

  bool processHop(HopToExecutorInst *hop);
  SILValue emitGetExecutor(SILLocation loc, SILValue actor);

public:
  LowerHopToActor(SILFunction *f, DominanceInfo *dominance)
    : F(f), B(*F), Dominance(dominance) { }

  /// The entry point to the transformation.
  bool run();
};

bool LowerHopToActor::run() {
  bool changed = false;

  auto runOnBlock = [&](SILBasicBlock *block) {
    for (auto ii = block->begin(), ie = block->end(); ii != ie; ) {
      SILInstruction *inst = &*ii++;
      if (auto *hop = dyn_cast<HopToExecutorInst>(inst)) {
        changed |= processHop(hop);
      }
    }
  };
  runInDominanceOrderWithScopes(Dominance, runOnBlock, ExecutorForActor);

  return changed;
}

/// Search for hop_to_executor instructions with actor-typed operands.
bool LowerHopToActor::processHop(HopToExecutorInst *hop) {
  auto actor = hop->getTargetExecutor();

  // Ignore hops that are already to Builtin.Executor.
  if (actor->getType().is<BuiltinExecutorType>())
    return false;

  B.setInsertionPoint(hop);

  // Get the dominating executor value for this actor, if available,
  // or else emit code to derive it.
  SILValue executor = ExecutorForActor.lookup(actor);
  if (!executor) {
    executor = emitGetExecutor(hop->getLoc(), actor);
    ExecutorForActor.insert(actor, executor);
  }

  B.createHopToExecutor(hop->getLoc(), executor);

  hop->eraseFromParent();

  return true;
}

SILValue LowerHopToActor::emitGetExecutor(SILLocation loc, SILValue actor) {
  // This is okay because actor types have to be classes and so never
  // have multiple abstraction patterns.
  CanType actorType = actor->getType().getASTType();

  auto &ctx = F->getASTContext();
  auto builtinName = ctx.getIdentifier(
    getBuiltinName(BuiltinValueKind::BuildSerialExecutorRef));
  auto builtinDecl = cast<FuncDecl>(getBuiltinValueDecl(ctx, builtinName));
  auto resultType = SILType::getPrimitiveObjectType(ctx.TheExecutorType);
  auto subs = SubstitutionMap::get(builtinDecl->getGenericSignature(),
                                   {actorType}, {});

  // Use builSerialExecutorRef to cast the actor to Builtin.Executor.
  // TODO: use the Executor protocol
  auto unmarkedExecutor =
    B.createBuiltin(loc, builtinName, resultType, subs, {actor});

  // Mark the dependence of the resulting value on the actor value to
  // force the actor to stay alive.
  return B.createMarkDependence(loc, unmarkedExecutor, actor);
}

class LowerHopToActorPass : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    auto fn = getFunction();
    if (!fn->isAsync())
      return;

    auto domTree = getAnalysis<DominanceAnalysis>()->get(fn);
    LowerHopToActor pass(getFunction(), domTree);
    if (pass.run())
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};

} // end anonymous namespace

SILTransform *swift::createLowerHopToActor() {
  return new LowerHopToActorPass();
}
