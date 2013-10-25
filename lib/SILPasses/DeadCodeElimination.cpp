//===--- DeadCodeElimination.cpp - Promote alloc_box to alloc_stack ------===//
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
#define DEBUG_TYPE "dead-code-elimination"

#include "swift/AST/Diagnostics.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumBlocksRemoved, "Number of unreachable basic blocks removed");
STATISTIC(NumInstructionsRemoved, "Number of unreachable instructions removed");

typedef llvm::SmallPtrSet<SILBasicBlock*, 16> SILBasicBlockSet;

template<typename...T, typename...U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc,
                         diag, std::forward<U>(args)...);
}

/// Information about a folded conditional branch instruction: it's location
/// and whether the condition evaluated to true or false.
struct BranchFoldInfo {
  SILLocation Loc;
  bool CondIsAlwaysTrue;
};

/// \class UnreachableUserCodeReportingState Contains extra state we need to
/// communicate from condition branch folding stage to the unreachable blocks
/// removal stage of the path.
///
/// To report unreachable user code, we detect the blocks that contain user
/// code and are not reachable (along any of the preceeding paths). Note that we
/// only want to report the first statement on the unreachable path. Keeping
/// the info about which branch folding had produced the unreachable block makes
/// it possible.
class UnreachableUserCodeReportingState {
public:
  /// \brief The set of top-level blocks that became immediately unreachbale due
  /// to conditional branch folding, etc.
  ///
  /// This is a SetVector since several blocks may lead to the same error
  /// report and we iterate through these when producing the diagnostic.
  // TODO: this could be a vector.
  llvm::SetVector<const SILBasicBlock*> UnreachableBlocks;

  /// \brief The set of blocks in which we reported unreachable code errors.
  /// These are used to ensure that we don't issue duplicate reports.
  ///
  /// Note, this set is different from the UnreachableBlocks as these are the
  /// blocks that do contain user code and they might not be immediate sucessors
  /// of a folded branch.
  llvm::SmallPtrSet<const SILBasicBlock*, 2> BlocksWithErrors;

  /// A map from the UnreachableBlocks to the folded conditional branchs
  /// that caused each of them to be unreachable. This extra info is used to
  /// enhance the diagnostics.
  llvm::DenseMap<const SILBasicBlock*, BranchFoldInfo> FoldInfoMap;
};


/// \brief Deletes the instrcutions in the set and any instructions that could
/// become dead after their removal.
///
/// Returns true if more instructions were determined to be dead and deleted.
static bool eraseAndCleanup(const llvm::DenseSet<SILInstruction*> &ToBeDeleted){
  bool AdditionalChanged = false;

  // First, drop references that keep other instructions live.
  llvm::DenseSet<SILInstruction*> PossiblyDead;
  for (auto II = ToBeDeleted.begin(), EI = ToBeDeleted.end(); II != EI; ++II) {

    // Deleting instructions might make their operands dead, let's collect them.
    SILInstruction* DI = *II;
    ArrayRef<Operand> Ops = DI->getAllOperands();
    for (auto OpI = Ops.begin(), OpE = Ops.end(); OpI != OpE; ++OpI) {
      SILInstruction *V = dyn_cast_or_null<SILInstruction>(OpI->get().getDef());
      // If the instruction will be deleted, no need to check if it is dead.
      if (V && !ToBeDeleted.count(V))
        PossiblyDead.insert(V);
    }

    // Drop references for all the instrcutions that will be deleted.
    DI->dropAllReferences();
  }

  // Delete the "possibly dead" instructions if they are dead.
  for (auto II = PossiblyDead.begin(),
       EI = PossiblyDead.end(); II != EI; ++II)
    AdditionalChanged &= recursivelyDeleteTriviallyDeadInstructions(*II);

  // Delete the unreachable instructions.
  for (auto II = ToBeDeleted.begin(),
            EI = ToBeDeleted.end(); II != EI; ++II) {
    (*II)->eraseFromParent();
  }

  return AdditionalChanged;
}

/// \brief Deletes the instruction and any instructions that could become dead
/// after its removal.
static bool eraseAndCleanup(SILInstruction *I) {
  llvm::DenseSet<SILInstruction*> Set;
  Set.insert(I);
  return eraseAndCleanup(Set);
}

/// \brief Propagate/remove basic block input values when all predecessors
/// supply the same arguments.
static void propagateBasicBlockArgs(SILBasicBlock &BB) {
  // This functions would simplify the code as following:
  //
  //   bb0:
  //     br bb1(%1 : $Builtin.Int1, %2 : $Builtin.Int1)
  //   bb1:
  //     br bb1(%1 : $Builtin.Int1, %2 : $Builtin.Int1)
  //   bb2(%3 : $Builtin.Int1, %4 : $Builtin.Int1):
  //     use(%3 : $Builtin.Int1)
  //     use(%4 : $Builtin.Int1)
  // =>
  //   bb0:
  //     br bb1
  //   bb2:
  //     use(%1 : $Builtin.Int1)
  //     use(%2 : $Builtin.Int1)

  // If there are no predecessors or no arguments, there is nothing to do.
  if (BB.pred_empty() || BB.bbarg_empty())
    return;

  // Check if all the predecessors supply the same arguments to the BB.
  SmallVector<SILValue, 4> Args;
  bool checkArgs = false;
  for (SILBasicBlock::pred_iterator PI = BB.pred_begin(), PE = BB.pred_end();
       PI != PE; ++PI) {
    SILBasicBlock *PredB = *PI;

    // We are only simplifying branch instructions.
    if (BranchInst *BI = dyn_cast<BranchInst>(PredB->getTerminator())) {
      unsigned Idx = 0;
      assert(!BI->getArgs().empty());
      for (OperandValueArrayRef::iterator AI = BI->getArgs().begin(),
                                          AE = BI->getArgs().end();
                                          AI != AE; ++AI, ++Idx) {
        // When processing the first predecessor, record the arguments.
        if (!checkArgs)
          Args.push_back(*AI);
        else
          // On each subsequent predecessor, check the arguments.
          if (Args[Idx] != *AI)
            return;
      }

    // If the terminator is not a branch instruction, do not simplify.
    } else {
      return;
    }

    // After the first branch is processed, the arguments vector is poulated.
    assert(Args.size() > 0);
    checkArgs = true;
  }

  // If we've reached this point, the optimization is valid, so optimize.
  // We know that the incomming arguments from all predecessors are the same,
  // so just use them directly and remove the basic block paramters.

  // Drop the arguments from the branch instructions by creating a new branch
  // instruction and deleting the old one.
  llvm::DenseSet<SILInstruction*> ToBeDeleted;
  for (SILBasicBlock::pred_iterator PI = BB.pred_begin(), PE = BB.pred_end();
       PI != PE; ++PI) {
    SILBasicBlock *PredB = *PI;
    BranchInst *BI = cast<BranchInst>(PredB->getTerminator());
    SILBuilder Bldr(PredB);
    Bldr.createBranch(BI->getLoc(), BI->getDestBB());
    ToBeDeleted.insert(BI);
  }

  // Drop the paranters from basic blocks and replace all uses with the passed
  // in arguments.
  unsigned Idx = 0;
  for (SILBasicBlock::bbarg_iterator AI = BB.bbarg_begin(),
                                     AE = BB.bbarg_end();
                                     AI != AE; ++AI, ++Idx) {
    // FIXME: These could be further propagatable now, we might want to move
    // this to CCP and trigger another round of copy propagation.
    SILArgument *Arg = *AI;

    // We were able to fold, so all users should use the new folded value.
    assert(Arg->getTypes().size() == 1 &&
           "Currently, we only support single result instructions.");
    SILValue(Arg).replaceAllUsesWith(Args[Idx]);

    // Remove args from the block.
    BB.dropAllArgs();
  }

  // The old branch instructions are no longer used, erase them.
  eraseAndCleanup(ToBeDeleted);
}

static bool constantFoldTerminator(SILBasicBlock &BB,
                                   UnreachableUserCodeReportingState *State) {
  TermInst *TI = BB.getTerminator();

  // Process conditional branches with constant conditions.
  if (CondBranchInst *CBI = dyn_cast<CondBranchInst>(TI)) {
    SILValue V = CBI->getCondition();
    SILInstruction *CondI = dyn_cast<SILInstruction>(V.getDef());
    SILLocation Loc = CBI->getLoc();

    if (IntegerLiteralInst *ConstCond =
          dyn_cast_or_null<IntegerLiteralInst>(CondI)) {
      SILBuilder B(&BB);

      // Determine which of the successors is unreachable and create a new
      // terminator that only branches to the reachable sucessor.
      SILBasicBlock *UnreachableBlock = nullptr;
      bool CondIsAlwaysTrue = false;
      if (ConstCond->getValue() == APInt(1, /*value*/ 0, false)) {
        B.createBranch(Loc,
                       CBI->getFalseBB(), CBI->getFalseArgs());
        UnreachableBlock = CBI->getTrueBB();
      } else {
        assert(ConstCond->getValue() == APInt(1, /*value*/ 1, false) &&
               "Our representation of true/false does not match.");
        B.createBranch(Loc,
                       CBI->getTrueBB(), CBI->getTrueArgs());
        UnreachableBlock = CBI->getFalseBB();
        CondIsAlwaysTrue = true;
      }
      eraseAndCleanup(TI);

      // Produce an unreachable code warning for this basic block if it
      // contains user code (only if we are not within an inlined function or a
      // template instantiation).
      // FIXME: Do not report if we are within a template instatiation.
      if (Loc.is<RegularLocation>() && State &&
          !State->UnreachableBlocks.count(UnreachableBlock)) {
        // If this is the first time we see this unreachable block, store it
        // along with the folded branch info.
        State->UnreachableBlocks.insert(UnreachableBlock);
        State->FoldInfoMap.insert(
          std::pair<const SILBasicBlock*,
                    BranchFoldInfo>(UnreachableBlock,
                                    BranchFoldInfo{Loc, CondIsAlwaysTrue}));
      }

      return true;
    }
  }

  // Constant fold switch enum.
  //   %1 = enum $Bool, #Bool.false!unionelt
  //   switch_enum %1 : $Bool, case #Bool.true!unionelt: bb1,
  //                            case #Bool.false!unionelt: bb2
  // =>
  //   br bb2
  if (SwitchEnumInst *SUI = dyn_cast<SwitchEnumInst>(TI)) {
    if (EnumInst *TheEnum = dyn_cast<EnumInst>(SUI->getOperand().getDef())) {
      const EnumElementDecl *TheEnumElem = TheEnum->getElement();
      SILBasicBlock *TheSuccessorBlock = nullptr;
      int ReachableBlockIdx = -1;
      for (unsigned Idx = 0; Idx < SUI->getNumCases(); ++Idx) {
        const EnumElementDecl *EI;
        SILBasicBlock *BI;
        llvm::tie(EI, BI) = SUI->getCase(Idx);
        if (EI == TheEnumElem) {
          TheSuccessorBlock = BI;
          ReachableBlockIdx = Idx;
          break;
        }
      }

      if (!TheSuccessorBlock)
        if (SUI->hasDefault()) {
          SILBasicBlock *DB= SUI->getDefaultBB();
          if (!isa<UnreachableInst>(DB->getTerminator())) {
            TheSuccessorBlock = DB;
            ReachableBlockIdx = SUI->getNumCases();
          }
        }

      // Not fully covered switches will be diagnosed later. SILGen represnets
      // them with a Default basic block with an unrechable instruction.
      // We are going to produce an error on all unreachable instructions not
      // eliminated by DCE.
      if (!TheSuccessorBlock)
        return false;

      // Replace the switch with a branch to the TheSuccessorBlock.
      SILBuilder B(&BB);
      SILLocation Loc = TI->getLoc();
      if (TheEnum->hasOperand())
        B.createBranch(Loc, TheSuccessorBlock,
                       TheEnum->getOperand());
      else
        B.createBranch(Loc, TheSuccessorBlock);

      // Produce diagnostic info if we are not within an inlined function or
      // template instantiation.
      // FIXME: Do not report if we are within a template instatiation.
      assert(ReachableBlockIdx >= 0);
      if (Loc.is<RegularLocation>() && State) {
        // Find the first unreachable block in the switch so that we could use
        // it for better diagnostics.
        SILBasicBlock *UnreachableBlock = nullptr;
        if (SUI->getNumCases() > 1) {
          // More than one case.
          UnreachableBlock =
            (ReachableBlockIdx == 0) ? SUI->getCase(1).second:
                                       SUI->getCase(0).second;
        } else {
          if (SUI->getNumCases() == 1 && SUI->hasDefault()) {
            // One case and a default.
            UnreachableBlock =
              (ReachableBlockIdx == 0) ? SUI->getDefaultBB():
                                         SUI->getCase(0).second;
          }
        }

        // Generate diagnostic info.
        if (UnreachableBlock &&
            !State->UnreachableBlocks.count(UnreachableBlock)) {
          State->UnreachableBlocks.insert(UnreachableBlock);
          State->FoldInfoMap.insert(
            std::pair<const SILBasicBlock*,
                      BranchFoldInfo>(UnreachableBlock,
                                      BranchFoldInfo{Loc, true}));
        }
      }

      eraseAndCleanup(TI);
      return true;
    }
  }

  // Constant fold switch int.
  //   %1 = integer_literal $Builtin.Int64, 2
  //   switch_int %1 : $Builtin.Int64, case 1: bb1, case 2: bb2
  // =>
  //   br bb2
  if (SwitchIntInst *SUI = dyn_cast<SwitchIntInst>(TI)) {
    if (IntegerLiteralInst *SwitchVal =
          dyn_cast<IntegerLiteralInst>(SUI->getOperand().getDef())) {
      SILBasicBlock *TheSuccessorBlock = 0;
      for (unsigned Idx = 0; Idx < SUI->getNumCases(); ++Idx) {
        APInt EI;
        SILBasicBlock *BI;
        llvm::tie(EI, BI) = SUI->getCase(Idx);
        if (EI == SwitchVal->getValue())
          TheSuccessorBlock = BI;
      }

      if (!TheSuccessorBlock)
        if (SUI->hasDefault())
          TheSuccessorBlock = SUI->getDefaultBB();

      // Add the branch instruction with the block.
      if (TheSuccessorBlock) {
        SILBuilder B(&BB);
        B.createBranch(TI->getLoc(), TheSuccessorBlock);
        eraseAndCleanup(TI);
        return true;
      }
    }
  }

  return false;
}

static bool isCallToNoReturn(const ApplyInst *AI, SILBasicBlock &BB) {
  return AI->getCallee().getType().castTo<AnyFunctionType>()->isNoReturn();
}

static bool simplifyBlocksWithCallsToNoReturn(SILBasicBlock &BB) {
  auto I = BB.begin(), E = BB.end();
  bool FoundNoReturnCall = false;

  // Collection of all instructions that should be deleted.
  llvm::DenseSet<SILInstruction*> ToBeDeleted;

  // Does this block conatin a call to a noreturn function?
  while (I != E) {
    auto CurrentInst = I;
    // Move the iterator before we remove instructions to avoid iterator
    // invalidation issues.
    ++I;

    // Remove all instructions following the noreturn call.
    if (FoundNoReturnCall) {

      // We will need to delete the instruction later on.
      ToBeDeleted.insert(CurrentInst);

      NumInstructionsRemoved++;
      continue;
    }

    if (ApplyInst *AI = dyn_cast<ApplyInst>(CurrentInst)) {
      if (isCallToNoReturn(AI, BB)) {
        FoundNoReturnCall = true;
        // FIXME: Diagnose unreachable code if the call is followed by anything
        // but implicit return.
      }
    }
  }

  if (!FoundNoReturnCall)
    return false;

  eraseAndCleanup(ToBeDeleted);

  // Add an unreachable terminator. The terminator has an invalid source
  // location to signal to the DataflowDiagnostic pass that this code does
  // not correspond to user code.
  SILBuilder B(&BB);
  B.createUnreachable(ArtificialUnreachableLocation());

  return true;
}

/// \brief Issue an "unreachable code" diagnostic if the blocks contains or
/// leads to another block that contains user code.
///
/// Note, we rely on SILLocation inforamtion to determine if SILInstructions
/// correspond to user code.
static bool diagnoseUnreachableBlock(const SILBasicBlock &B,
                                     SILModule &M,
                                     const SILBasicBlockSet &Reachable,
                                     UnreachableUserCodeReportingState *State,
                                     const SILBasicBlock *TopLevelB = nullptr) {
  assert(State);
  for (auto I = B.begin(), E = B.end(); I != E; ++I) {
    SILLocation Loc = I->getLoc();

    // Skip branch instructions. These could belong to the control flow
    // statement we are folding (ex: while loop).
    // Also skip unreachable instructions, they are "expected" in unreachable
    // blocks.
    if ((isa<BranchInst>(I) || isa<UnreachableInst>(I)) &&
        Loc.is<RegularLocation>())
      break;

    // If we've reached an implicit return, we have not found any user code and
    // can stop searching for it.
    if (Loc.is<ImplicitReturnLocation>())
      return false;

    // If the instruction corresponds to user-written return or some other
    // statement, we know it corresponds to user code.
    // Check BlocksWithErrors to make sure we don't report an error twice.
    if ((Loc.is<RegularLocation>() || Loc.is<ReturnLocation>()) &&
        !State->BlocksWithErrors.count(&B)) {

      // Emit the diagnostic.
      auto BrInfoIter = State->FoldInfoMap.find(TopLevelB);
      assert(BrInfoIter != State->FoldInfoMap.end());
      auto BrInfo = BrInfoIter->second;

      // If we are warning about a switch condition being a constant, the main
      // emphasis should be on the condition (to ensure we have a single
      // message per switch).
      if (const SwitchStmt *SS = BrInfo.Loc.getAsASTNode<SwitchStmt>()) {
        const Expr *SE = SS->getSubjectExpr();
        diagnose(M.getASTContext(), SE->getLoc(), diag::switch_on_a_constant);
        diagnose(M.getASTContext(), Loc.getSourceLoc(),
                 diag::unreachable_code_note);
        return true;
      }

      // Otherwise, emit the diagnostic on the unreachable block and emit the
      // note on the branch responsible for the unreachable code.
      diagnose(M.getASTContext(), Loc.getSourceLoc(),
               diag::unreachable_code);
      diagnose(M.getASTContext(), BrInfo.Loc.getSourceLoc(),
        diag::unreachable_code_branch, BrInfo.CondIsAlwaysTrue);

      // Record that we've reported this unreachable block to avoid duplicates
      // in the future.
      State->BlocksWithErrors.insert(&B);
      return true;
    }
  }

  // This block could be empty if it's terminator has been folded.
  if (B.empty())
    return false;

  // If we have not found user code in this block, inspect it's successors.
  // Check if at least one of the sucessors contains user code.
  for (auto I = B.succ_begin(), E = B.succ_end(); I != E; ++I) {
    SILBasicBlock *SB = *I;
    bool HasReachablePred = false;
    for (auto PI = SB->pred_begin(), PE = SB->pred_end(); PI != PE; ++PI) {
      if (Reachable.count(*PI))
        HasReachablePred = true;
    }

    // If all of the predecessors of this sucessor are unreachable, check if
    // it contains user code.
    if (!HasReachablePred && diagnoseUnreachableBlock(*SB, M, Reachable,
                                                      State, TopLevelB))
      return true;
  }
  
  return false;
}

static bool removeUnreachableBlocks(SILFunction &F, SILModule &M,
                                    UnreachableUserCodeReportingState *State) {
  if (F.empty())
    return false;

  SILBasicBlockSet Reachable;
  SmallVector<SILBasicBlock*, 128> Worklist;
  Worklist.push_back(&F.front());
  Reachable.insert(&F.front());

  // Collect all reachable blocks by walking the successors.
  do {
    SILBasicBlock *BB = Worklist.pop_back_val();
    for (auto SI = BB->succ_begin(), SE = BB->succ_end(); SI != SE; ++SI) {
      if (Reachable.insert(*SI))
        Worklist.push_back(*SI);
    }
  } while (!Worklist.empty());
  assert(Reachable.size() <= F.size());

  // If everything is reachable, we are done.
  if (Reachable.size() == F.size())
    return false;

  // Diagnose user written unreachable code.
  if (State) {
    for (auto BI = State->UnreachableBlocks.begin(),
              BE = State->UnreachableBlocks.end(); BI != BE; ++BI) {
      diagnoseUnreachableBlock(**BI, M, Reachable, State, *BI);
    }
  }

  // Remove references from the dead blocks.
  for (auto I = F.begin(), E = F.end(); I != E; ++I) {
    SILBasicBlock *BB = I;
    if (Reachable.count(BB))
      continue;

    // Drop references to other blocks.
    eraseAndCleanup(BB->getTerminator());
  }

  // Delete dead instrcutions and everything that could become dead after
  // their deletion.
  llvm::DenseSet<SILInstruction*> ToBeDeleted;
  for (auto BI = F.begin(), BE = F.end(); BI != BE; ++BI)
    if (!Reachable.count(BI))
      for (auto I = BI->begin(), E = BI->end(); I != E; ++I)
        ToBeDeleted.insert(&*I);
  eraseAndCleanup(ToBeDeleted);

  // Delete the dead blocks.
  for (auto I = F.begin(), E = F.end(); I != E;)
    if (!Reachable.count(I)) {
      I = F.getBlocks().erase(I);
      NumBlocksRemoved++;
    } else
      ++I;

  return true;
}

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILDeadCodeElimination(SILModule *M) {
  for (auto &Fn : *M) {
    DEBUG(llvm::errs() << "*** Dead Code Elimination processing: "
          << Fn.getName() << "\n");

    UnreachableUserCodeReportingState State;

    for (auto &BB : Fn) {
      // Simplify the blocks with terminators that rely on constant conditions.
      if (constantFoldTerminator(BB, &State))
        continue;

      // Remove instructions from the basic block after a call to a noreturn
      // function.
      if (simplifyBlocksWithCallsToNoReturn(BB))
        continue;
    }

    // Remove unreachable blocks.
    removeUnreachableBlocks(Fn, *M, &State);

    for (auto &BB : Fn) {
      propagateBasicBlockArgs(BB);
    }

    for (auto &BB : Fn) {
      // Simplify the blocks with terminators that rely on constant conditions.
      if (constantFoldTerminator(BB, &State)) {
        continue;
      }
      // Remove instructions from the basic block after a call to a noreturn
      // function.
      if (simplifyBlocksWithCallsToNoReturn(BB))
        continue;
    }

    // Remove unreachable blocks.
    removeUnreachableBlocks(Fn, *M, &State);
  }
}
