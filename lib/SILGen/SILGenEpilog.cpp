//===--- SILGenEpilog.cpp - Function epilogue emission --------------------===//
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

#include "SILGen.h"
#include "SILGenFunction.h"
#include "ASTVisitor.h"
#include "swift/SIL/SILArgument.h"

using namespace swift;
using namespace Lowering;

void SILGenFunction::prepareEpilog(Type resultType, bool isThrowing,
                                   CleanupLocation CleanupL) {
  auto *epilogBB = createBasicBlock();

  // If we have any direct results, receive them via BB arguments.
  // But callers can disable this by passing a null result type.
  if (resultType) {
    auto fnConv = F.getConventions();
    // Set NeedsReturn for indirect or direct results. This ensures that SILGen
    // emits unreachable if there is no source level return.
    NeedsReturn = (fnConv.funcTy->getNumResults() != 0);
    for (auto directResult : fnConv.getDirectSILResults()) {
      SILType resultType =
          F.mapTypeIntoContext(fnConv.getSILType(directResult));
      epilogBB->createPHIArgument(resultType, ValueOwnershipKind::Owned);
    }
  }

  ReturnDest = JumpDest(epilogBB, getCleanupsDepth(), CleanupL);

  if (isThrowing) {
    prepareRethrowEpilog(CleanupL);
  }

  if (F.getLoweredFunctionType()->isCoroutine()) {
    prepareCoroutineUnwindEpilog(CleanupL);
  }
}

void SILGenFunction::prepareRethrowEpilog(CleanupLocation cleanupLoc) {
  auto exnType = SILType::getExceptionType(getASTContext());
  SILBasicBlock *rethrowBB = createBasicBlock(FunctionSection::Postmatter);
  rethrowBB->createPHIArgument(exnType, ValueOwnershipKind::Owned);
  ThrowDest = JumpDest(rethrowBB, getCleanupsDepth(), cleanupLoc);
}

void SILGenFunction::prepareCoroutineUnwindEpilog(CleanupLocation cleanupLoc) {
  SILBasicBlock *unwindBB = createBasicBlock(FunctionSection::Postmatter);
  CoroutineUnwindDest = JumpDest(unwindBB, getCleanupsDepth(), cleanupLoc);
}

/// Given a list of direct results, form the direct result value.
///
/// Note that this intentionally loses any tuple sub-structure of the
/// formal result type.
static SILValue buildReturnValue(SILGenFunction &SGF, SILLocation loc,
                                 ArrayRef<SILValue> directResults) {
  if (directResults.size() == 1)
    return directResults[0];

  SmallVector<TupleTypeElt, 4> eltTypes;
  for (auto elt : directResults)
    eltTypes.push_back(elt->getType().getASTType());
  auto resultType = SILType::getPrimitiveObjectType(
    CanType(TupleType::get(eltTypes, SGF.getASTContext())));
  return SGF.B.createTuple(loc, resultType, directResults);
}

static Optional<SILLocation>
prepareForEpilogBlockEmission(SILGenFunction &SGF, SILLocation topLevel,
                              SILBasicBlock *epilogBB,
                              SmallVectorImpl<SILValue> &directResults) {
  SILLocation implicitReturnFromTopLevel =
      ImplicitReturnLocation::getImplicitReturnLoc(topLevel);

  // If the current BB we are inserting into isn't terminated, and we require a
  // return, then we
  // are not allowed to fall off the end of the function and can't reach here.
  if (SGF.NeedsReturn && SGF.B.hasValidInsertionPoint())
    SGF.B.createUnreachable(implicitReturnFromTopLevel);

  if (epilogBB->pred_empty()) {
    // If the epilog was not branched to at all, kill the BB and
    // just emit the epilog into the current BB.
    while (!epilogBB->empty())
      epilogBB->back().eraseFromParent();
    SGF.eraseBasicBlock(epilogBB);

    // If the current bb is terminated then the epilog is just unreachable.
    if (!SGF.B.hasValidInsertionPoint())
      return None;

    // We emit the epilog at the current insertion point.
    return implicitReturnFromTopLevel;
  }

  if (std::next(epilogBB->pred_begin()) == epilogBB->pred_end() &&
      !SGF.B.hasValidInsertionPoint()) {
    // If the epilog has a single predecessor and there's no current insertion
    // point to fall through from, then we can weld the epilog to that
    // predecessor BB.

    // Steal the branch argument as the return value if present.
    SILBasicBlock *pred = *epilogBB->pred_begin();
    BranchInst *predBranch = cast<BranchInst>(pred->getTerminator());
    assert(predBranch->getArgs().size() == epilogBB->args_size() &&
           "epilog predecessor arguments does not match block params");

    for (auto index : indices(predBranch->getArgs())) {
      SILValue result = predBranch->getArgs()[index];
      directResults.push_back(result);
      epilogBB->getArgument(index)->replaceAllUsesWith(result);
    }

    Optional<SILLocation> returnLoc;
    // If we are optimizing, we should use the return location from the single,
    // previously processed, return statement if any.
    if (predBranch->getLoc().is<ReturnLocation>()) {
      returnLoc = predBranch->getLoc();
    } else {
      returnLoc = implicitReturnFromTopLevel;
    }

    // Kill the branch to the now-dead epilog BB.
    pred->erase(predBranch);

    // Move any instructions from the EpilogBB to the end of the 'pred' block.
    pred->spliceAtEnd(epilogBB);

    // Finally we can erase the epilog BB.
    SGF.eraseBasicBlock(epilogBB);

    // Emit the epilog into its former predecessor.
    SGF.B.setInsertionPoint(pred);
    return returnLoc;
  }

  // Move the epilog block to the end of the ordinary section.
  auto endOfOrdinarySection = SGF.StartOfPostmatter;
  SGF.B.moveBlockTo(epilogBB, endOfOrdinarySection);

  // Emit the epilog into the epilog bb. Its arguments are the
  // direct results.
  directResults.append(epilogBB->args_begin(), epilogBB->args_end());

  // If we are falling through from the current block, the return is implicit.
  SGF.B.emitBlock(epilogBB, implicitReturnFromTopLevel);

  // If the return location is known to be that of an already
  // processed return, use it. (This will get triggered when the
  // epilog logic is simplified.)
  //
  // Otherwise make the ret instruction part of the cleanups.
  auto cleanupLoc = CleanupLocation::get(topLevel);
  return cleanupLoc;
}

std::pair<Optional<SILValue>, SILLocation>
SILGenFunction::emitEpilogBB(SILLocation topLevel) {
  assert(ReturnDest.getBlock() && "no epilog bb prepared?!");
  SILBasicBlock *epilogBB = ReturnDest.getBlock();
  SmallVector<SILValue, 8> directResults;

  // Prepare the epilog block for emission. If we need to actually emit the
  // block, we return a real SILLocation. Otherwise, the epilog block is
  // actually unreachable and we can just return early.
  auto returnLoc =
      prepareForEpilogBlockEmission(*this, topLevel, epilogBB, directResults);
  if (!returnLoc.hasValue()) {
    return {None, topLevel};
  }

  // Emit top-level cleanups into the epilog block.
  assert(!Cleanups.hasAnyActiveCleanups(getCleanupsDepth(),
                                        ReturnDest.getDepth()) &&
         "emitting epilog in wrong scope");

  auto cleanupLoc = CleanupLocation::get(topLevel);
  Cleanups.emitCleanupsForReturn(cleanupLoc, NotForUnwind);

  // Build the return value.  We don't do this if there are no direct
  // results; this can happen for void functions, but also happens when
  // prepareEpilog was asked to not add result arguments to the epilog
  // block.
  SILValue returnValue;
  if (!directResults.empty()) {
    assert(directResults.size() == F.getConventions().getNumDirectSILResults());
    returnValue = buildReturnValue(*this, topLevel, directResults);
  }

  return {returnValue, *returnLoc};
}

SILLocation SILGenFunction::
emitEpilog(SILLocation TopLevel, bool UsesCustomEpilog) {
  Optional<SILValue> maybeReturnValue;
  SILLocation returnLoc(TopLevel);
  std::tie(maybeReturnValue, returnLoc) = emitEpilogBB(TopLevel);

  SILBasicBlock *ResultBB = nullptr;
  
  if (!maybeReturnValue) {
    // Nothing to do.
  } else if (UsesCustomEpilog) {
    // If the epilog is reachable, and the caller provided an epilog, just
    // remember the block so the caller can continue it.
    ResultBB = B.getInsertionBB();
    assert(ResultBB && "Didn't have an epilog block?");
    B.clearInsertionPoint();
  } else {
    // Otherwise, if the epilog block is reachable, return the return value.
    SILValue returnValue = *maybeReturnValue;

    // Return () if no return value was given.
    if (!returnValue)
      returnValue = emitEmptyTuple(CleanupLocation::get(TopLevel));

    B.createReturn(returnLoc, returnValue);
  }
  
  emitRethrowEpilog(TopLevel);
  emitCoroutineUnwindEpilog(TopLevel);
  
  if (ResultBB)
    B.setInsertionPoint(ResultBB);
  
  return returnLoc;
}

static bool prepareExtraEpilog(SILGenFunction &SGF, JumpDest &dest,
                               SILLocation &loc, SILValue *arg) {
  assert(!SGF.B.hasValidInsertionPoint());

  // If we don't have a destination, we don't need to emit the epilog.
  if (!dest.isValid())
    return false;

  // If the destination isn't used, we don't need to emit the epilog.
  SILBasicBlock *epilogBB = dest.getBlock();
  auto pi = epilogBB->pred_begin(), pe = epilogBB->pred_end();
  if (pi == pe) {
    dest = JumpDest::invalid();
    SGF.eraseBasicBlock(epilogBB);
    return false;
  }

  assert(epilogBB->getNumArguments() <= 1);
  assert((epilogBB->getNumArguments() == 1) == (arg != nullptr));
  if (arg) *arg = epilogBB->args_begin()[0];

  bool reposition = true;

  // If the destination has a single branch predecessor,
  // consider emitting the epilog into it.
  SILBasicBlock *predBB = *pi;
  if (++pi == pe) {
    if (auto branch = dyn_cast<BranchInst>(predBB->getTerminator())) {
      assert(branch->getArgs().size() == epilogBB->getNumArguments());

      // Save the location and operand information from the branch,
      // then destroy it.
      loc = branch->getLoc();
      if (arg) *arg = branch->getArgs()[0];
      predBB->erase(branch);

      // Erase the rethrow block.
      SGF.eraseBasicBlock(epilogBB);
      epilogBB = predBB;
      reposition = false;
    }
  }

  // Reposition the block to the end of the postmatter section
  // unless we're emitting into a single predecessor.
  if (reposition) {
    SGF.B.moveBlockTo(epilogBB, SGF.F.end());
  }

  SGF.B.setInsertionPoint(epilogBB);

  return true;
}

void SILGenFunction::emitRethrowEpilog(SILLocation topLevel) {
  SILValue exn;
  SILLocation throwLoc = topLevel;
  if (!prepareExtraEpilog(*this, ThrowDest, throwLoc, &exn))
    return;

  Cleanups.emitCleanupsForReturn(ThrowDest.getCleanupLocation(), IsForUnwind);

  B.createThrow(throwLoc, exn);

  ThrowDest = JumpDest::invalid();
}

void SILGenFunction::emitCoroutineUnwindEpilog(SILLocation topLevel) {
  SILLocation unwindLoc = topLevel;
  if (!prepareExtraEpilog(*this, CoroutineUnwindDest, unwindLoc, nullptr))
    return;

  Cleanups.emitCleanupsForReturn(CoroutineUnwindDest.getCleanupLocation(),
                                 IsForUnwind);

  B.createUnwind(unwindLoc);

  CoroutineUnwindDest = JumpDest::invalid();
}
