//===--- SILGenEpilog.cpp - Function epilogue emission --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

  // If we have a non-null, non-void, non-address-only return type, receive the
  // return value via a BB argument.
  NeedsReturn = resultType && !resultType->isVoid();
  if (NeedsReturn) {
    auto &resultTI = getTypeLowering(resultType);
    if (!resultTI.isAddressOnly())
      new (F.getModule()) SILArgument(epilogBB, resultTI.getLoweredType());
  }
  ReturnDest = JumpDest(epilogBB, getCleanupsDepth(), CleanupL);

  if (isThrowing) {
    auto exnType = SILType::getExceptionType(getASTContext());
    SILBasicBlock *rethrowBB = createBasicBlock(FunctionSection::Postmatter);
    new (F.getModule()) SILArgument(rethrowBB, exnType);
    ThrowDest = JumpDest(rethrowBB, getCleanupsDepth(), CleanupL);
  }
}

std::pair<Optional<SILValue>, SILLocation>
SILGenFunction::emitEpilogBB(SILLocation TopLevel) {
  assert(ReturnDest.getBlock() && "no epilog bb prepared?!");
  SILBasicBlock *epilogBB = ReturnDest.getBlock();
  SILLocation ImplicitReturnFromTopLevel =
    ImplicitReturnLocation::getImplicitReturnLoc(TopLevel);
  SILValue returnValue;
  Optional<SILLocation> returnLoc = None;

  // If the current BB isn't terminated, and we require a return, then we
  // are not allowed to fall off the end of the function and can't reach here.
  if (NeedsReturn && B.hasValidInsertionPoint())
    B.createUnreachable(ImplicitReturnFromTopLevel);

  if (epilogBB->pred_empty()) {
    bool hadArg = !epilogBB->bbarg_empty();

    // If the epilog was not branched to at all, kill the BB and
    // just emit the epilog into the current BB.
    while (!epilogBB->empty())
      epilogBB->getInstList().back().eraseFromParent();
    eraseBasicBlock(epilogBB);

    // If the current bb is terminated then the epilog is just unreachable.
    if (!B.hasValidInsertionPoint())
      return { None, TopLevel };
    // We emit the epilog at the current insertion point.
    assert(!hadArg && "NeedsReturn is false but epilog had argument?!");
    (void)hadArg;
    returnLoc = ImplicitReturnFromTopLevel;

  } else if (std::next(epilogBB->pred_begin()) == epilogBB->pred_end()
             && !B.hasValidInsertionPoint()) {
    // If the epilog has a single predecessor and there's no current insertion
    // point to fall through from, then we can weld the epilog to that
    // predecessor BB.
    bool needsArg = false;
    if (!epilogBB->bbarg_empty()) {
      assert(epilogBB->bbarg_size() == 1 && "epilog should take 0 or 1 args");
      needsArg = true;
    }

    // Steal the branch argument as the return value if present.
    SILBasicBlock *pred = *epilogBB->pred_begin();
    BranchInst *predBranch = cast<BranchInst>(pred->getTerminator());
    assert(predBranch->getArgs().size() == (needsArg ? 1 : 0) &&
           "epilog predecessor arguments does not match block params");

    if (needsArg) {
      returnValue = predBranch->getArgs()[0];
      // RAUW the old BB argument (if any) with the new value.
      SILValue(*epilogBB->bbarg_begin(),0).replaceAllUsesWith(returnValue);
    }

    // If we are optimizing, we should use the return location from the single,
    // previously processed, return statement if any.
    if (predBranch->getLoc().is<ReturnLocation>()) {
      returnLoc = predBranch->getLoc();
    } else {
      returnLoc = ImplicitReturnFromTopLevel;
    }
    
    // Kill the branch to the now-dead epilog BB.
    pred->getInstList().erase(predBranch);

    // Move any instructions from the EpilogBB to the end of the 'pred' block.
    pred->getInstList().splice(pred->end(), epilogBB->getInstList());

    // Finally we can erase the epilog BB.
    eraseBasicBlock(epilogBB);

    // Emit the epilog into its former predecessor.
    B.setInsertionPoint(pred);
  } else {
    // Move the epilog block to the end of the ordinary section.
    auto endOfOrdinarySection =
      (StartOfPostmatter ? SILFunction::iterator(StartOfPostmatter) : F.end());
    B.moveBlockTo(epilogBB, endOfOrdinarySection);

    // Emit the epilog into the epilog bb. Its argument is the return value.
    if (!epilogBB->bbarg_empty()) {
      assert(epilogBB->bbarg_size() == 1 && "epilog should take 0 or 1 args");
      returnValue = epilogBB->bbarg_begin()[0];
    }

    // If we are falling through from the current block, the return is implicit.
    B.emitBlock(epilogBB, ImplicitReturnFromTopLevel);
  }
  
  // Emit top-level cleanups into the epilog block.
  assert(!Cleanups.hasAnyActiveCleanups(getCleanupsDepth(),
                                        ReturnDest.getDepth()) &&
         "emitting epilog in wrong scope");

  auto cleanupLoc = CleanupLocation::get(TopLevel);
  Cleanups.emitCleanupsForReturn(cleanupLoc);

  // If the return location is known to be that of an already
  // processed return, use it. (This will get triggered when the
  // epilog logic is simplified.)
  //
  // Otherwise make the ret instruction part of the cleanups.
  if (!returnLoc) returnLoc = cleanupLoc;

  return { returnValue, *returnLoc };
}

void SILGenFunction::emitEpilog(SILLocation TopLevel) {
  // Some callers will want to provide a custom epilog.  They do this by filling
  // it into the EpilogBB.  When they do that, just disable the default one.
  bool callerProvidedEpilog = !ReturnDest.getBlock()->empty();

  Optional<SILValue> maybeReturnValue;
  SILLocation returnLoc(TopLevel);
  std::tie(maybeReturnValue, returnLoc) = emitEpilogBB(TopLevel);

  SILBasicBlock *ResultBB = nullptr;
  
  if (!maybeReturnValue) {
    // Nothing to do.
  } else if (callerProvidedEpilog) {
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

    if (!MainScope)
      MainScope = F.getDebugScope();
    setDebugScopeForInsertedInstrs(MainScope);
  }
  
  emitRethrowEpilog(TopLevel);
  
  if (ResultBB)
    B.setInsertionPoint(ResultBB);
}

void SILGenFunction::emitRethrowEpilog(SILLocation topLevel) {
  assert(!B.hasValidInsertionPoint());

  // If we don't have a rethrow destination, we're done.
  if (!ThrowDest.isValid())
    return;

  // If the rethrow destination isn't used, we're done.
  SILBasicBlock *rethrowBB = ThrowDest.getBlock();
  if (rethrowBB->pred_empty()) {
    ThrowDest = JumpDest::invalid();
    eraseBasicBlock(rethrowBB);
    return;
  }

  SILLocation throwLoc = topLevel;
  SILValue exn = rethrowBB->bbarg_begin()[0];
  bool reposition = true;

  // If the rethrow destination has a single branch predecessor,
  // consider emitting the rethrow into it.
  SILBasicBlock *predBB = *rethrowBB->pred_begin();
  if (std::next(rethrowBB->pred_begin()) == rethrowBB->pred_end()) {
    if (auto branch = dyn_cast<BranchInst>(predBB->getTerminator())) {
      assert(branch->getArgs().size() == 1);

      // Save the location and operand information from the branch,
      // then destroy it.
      throwLoc = branch->getLoc();
      exn = branch->getArgs()[0];
      predBB->getInstList().erase(branch);

      // Erase the rethrow block.
      eraseBasicBlock(rethrowBB);
      rethrowBB = predBB;
      reposition = false;
    }
  }

  // Reposition the rethrow block to the end of the postmatter section
  // unless we're emitting into a single predecessor.
  if (reposition) {
    B.moveBlockTo(rethrowBB, F.end());
  }

  B.setInsertionPoint(rethrowBB);
  Cleanups.emitCleanupsForReturn(ThrowDest.getCleanupLocation());
  B.createThrow(throwLoc, exn);

  ThrowDest = JumpDest::invalid();
}
