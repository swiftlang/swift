//===-------- DiagnoseVarUsage.cpp - Perform var usage diagnostics --------===//
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
//
// This file implements a diagnostic pass to diagnose unused, never read, and
// never mutated values.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "diagnose-var-usage"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                          MARK: Utility Functions
//===----------------------------------------------------------------------===//

/// Returns the \c VarDecl this instruction belongs to or \c nullptr
static VarDecl *getVarDecl(SILInstruction *I) {
  if (auto Loc = I->getLoc()) {
    if (auto *VD = Loc.getAsASTNode<VarDecl>()) {
      return VD;
    }
  }
  return nullptr;
}

/// Returns whether this instruction belongs to a setter parameter \c newValue
static bool isSetterParam(SILInstruction *I) {
  auto *VD = getVarDecl(I);
  if (!VD) {
    return false;
  }
  if (auto PD = dyn_cast<ParamDecl>(VD)) {
    if (auto AD = dyn_cast<AccessorDecl>(VD->getDeclContext())) {
      if (AD->getAccessorKind() == AccessorKind::Set) {
        return true;
      }
    }
  }
  return false;
}

/// Returns the SILInstruction if it is part of a VarDecl we want to track.
static bool shouldTrackVarDecl(SILInstruction *I) {
  if (auto *VD = getVarDecl(I)) {
    
    if (VD->isCaptureList()) {
      return true;
    }
    
    if (isSetterParam(I)) {
      return true;
    }
    
    // If the variable is implicit, ignore it.
    if (VD->isImplicit() || VD->getLoc().isInvalid())
      return false;
    
    // If the variable is computed, ignore it.
    if (!VD->hasStorage())
      return false;
    
    // If the variable was invalid, ignore it and notice that the code is
    // malformed.
    if (VD->isInvalid()) {
      return false;
    }
    
    // If the variable is already unnamed, ignore it.
    if (!VD->hasName() || VD->getName().str() == "_")
      return false;
    
    return (VD->getDeclContext()->isLocalContext() &&
            !isa<ParamDecl>(VD));
  }
  return false;
}

/// Returns whether the instruction belongs to a self parameter
static bool isSelfParam(SILInstruction *I) {
  if (auto *VD = getVarDecl(I)) {
    return VD->isSelfParameter();
  }
  return false;
}

/// Returns the value referenced by the \c DebugValueInst
static SILValue getDebugVarValue(SILInstruction *I) {
  if (auto DVI = dyn_cast<DebugValueInst>(I)) {
    // DebugValueInst are unused, so use the instruction it references.
    if (SILValue Value = DVI->getOperand()) {
      // FIXME: what else can we step into?
      if (auto Inst = Value->getDefiningInstruction()) {
        if (auto LI = dyn_cast<LoadInst>(Inst)) {
          return LI->getOperand();
        }
      }
      return Value;
    }
  }
  return nullptr;
}

/// Returns the value referenced by the \c DebugValueAddrInst
static SILValue getDebugVarAddrValue(SILInstruction *I) {
  if (auto DVAI = dyn_cast<DebugValueAddrInst>(I)) {
    // DebugValueAddrInst are unused, so use the instruction it references.
    if (SILValue Value = DVAI->getOperand()) {
      // FIXME: what else?
      // Do we need this one?
      if (auto Inst = Value->getDefiningInstruction()) {
        if (auto LI = dyn_cast<LoadInst>(Inst)) {
          return LI->getOperand();
        }
      }
      return Value;
    }
  }
  return nullptr;
}

static bool shouldTrackDebugVar(SILInstruction *I) {
  if (auto *DVI = dyn_cast<DebugValueInst>(I)) {
    
    // Var Declaration is setter parameter 'newValue'
    if (isSetterParam(I)) {
      return true;
    }
    
    // Var Declaration is a parameter that we do not track
    if (DVI->getVarInfo().hasValue()) {
      if (DVI->getVarInfo().getValue().ArgNo) {
        
        // This debug var represents an argument, not the initial declaration
        return false;
      }
    }
    
    // Otherwise, this is just a local Var Declaration
    return shouldTrackVarDecl(DVI);
  }
  return false;
}

/// Checks that the \c First instruction comes before \c Second
static bool instructionPrecedes(SILInstruction *First,
                                SILInstruction *Second,
                                SILBasicBlock *BB) {
  auto i = BB->begin(), e = BB->end();
  while (i != e) {
    if (&*i == Second) {
      return false;
    } else if (&*i == First) {
      return true;
    }
    ++i;
  }
  return false;
}

/// Returns the entry instruction to begin diagnostics.
/// Returns \c nullptr if the instruction does not belong to a var
/// declaration that should be checked for diagnostics.
static SILInstruction *getEntryInst(SILInstruction *I) {
  
  // FIXME: VarDecl are lowered to AllocBoxInst or EnumInst
  if (!isa<AllocBoxInst>(I) &&
      !isa<AllocStackInst>(I) &&
      !isa<DebugValueInst>(I) &&
      !isa<EnumInst>(I)) {
    return nullptr;
  }
  
  if (isa<DebugValueInst>(I)) {
    if (shouldTrackDebugVar(I)) {
      if (isSelfParam(I)) {
        return nullptr;
      }
      return shouldTrackVarDecl(I) ? I : nullptr;
    }
    return nullptr;
  }
  
  return shouldTrackVarDecl(I) ? I : nullptr;
}

/// Returns true if an argument is inout
static bool argIsInout(SILArgumentConvention ArgConv) {
  switch (ArgConv) {
    case SILArgumentConvention::Indirect_Inout:
    case SILArgumentConvention::Indirect_InoutAliasable:
      return true;
    default:
      return false;
  }
}

/// Finds the corresponding SILArgument for a parameter in an apply
static SILArgument *getFunctionArgInst(SILInstruction *Apply,
                                       Operand *Op) {
  ApplySite AS;
  SILFunction *Fn;
  
  if (auto AI = dyn_cast<ApplyInst>(Apply)) {
    AS = ApplySite(AI);
    Fn = AI->getReferencedFunctionOrNull();
  } else if (auto PAI = dyn_cast<PartialApplyInst>(Apply)) {
    AS = ApplySite(PAI);
    Fn = PAI->getReferencedFunctionOrNull();
  } else if (auto TAI = dyn_cast<TryApplyInst>(Apply)) {
    AS = ApplySite(TAI);
    Fn = TAI->getReferencedFunctionOrNull();
  } else if (auto BAI = dyn_cast<BeginApplyInst>(Apply)) {
    AS = ApplySite(BAI);
    Fn = BAI->getReferencedFunctionOrNull();
  }
  
  if (AS && Fn && !Fn->isExternalDeclaration()) {
    if ((Op->getUser() == Apply) && AS.isArgumentOperand(*Op)) {
      unsigned index = AS.getCalleeArgIndex(*Op);
      auto bb = Fn->getEntryBlock();
      return bb->getArgument(index);
    }
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
//                          MARK: Def-Use Traversers
//===----------------------------------------------------------------------===//

class VariableModifyTraverser {
  
  SmallVector<SILValue, 32> Worklist;
  llvm::SetVector<SILValue> Seen;
  SILInstruction *EntryInst;
  
public:
  VariableModifyTraverser(SILInstruction *EntryInst) : EntryInst(EntryInst) {
    appendValues(EntryInst);
  }
  
private:
  
  /// Stage instruction results for traversal
  void appendValues(SILInstruction *I) {
    for (SILValue Value : I->getResults()) {
      appendValue(Value);
    }
  }
  
  /// Stage a value for traversal
  void appendValue(SILValue Value) {
    if (!Seen.count(Value)) {
      Worklist.push_back(Value);
      Seen.insert(Value);
    }
  }
  
  /// Determine if external SIL function modifies a value. Returns true if the
  /// parameter is an inout.
  static bool externalFunctionArgIsModified(SILInstruction *Apply,
                                            Operand *Op) {
    
    ApplySite AS;
    SILFunction *Fn;
    
    if (auto AI = dyn_cast<ApplyInst>(Apply)) {
      AS = ApplySite(AI);
      Fn = AI->getReferencedFunctionOrNull();
    } else if (auto PAI = dyn_cast<PartialApplyInst>(Apply)) {
      AS = ApplySite(PAI);
      Fn = PAI->getReferencedFunctionOrNull();
    } else if (auto TAI = dyn_cast<TryApplyInst>(Apply)) {
      AS = ApplySite(TAI);
      Fn = TAI->getReferencedFunctionOrNull();
    } else if (auto BAI = dyn_cast<BeginApplyInst>(Apply)) {
      AS = ApplySite(BAI);
      Fn = BAI->getReferencedFunctionOrNull();
    }
    
    if (AS && Fn && Fn->isExternalDeclaration()) {
      if ((Op->getUser() == Apply) && AS.isArgumentOperand(*Op)) {
        return argIsInout(AS.getArgumentConvention(*Op));
      }
    }
    return false;
  }
  
  /// Determines if an instruction terminates the traversal as a modier of
  /// the entry instruction.
  bool isDirectModier(SILInstruction *I, Operand *Op) {
    switch (I->getKind()) {
        
      case SILInstructionKind::BeginAccessInst:
        return dyn_cast<BeginAccessInst>(I)->getAccessKind() ==
        SILAccessKind::Modify;
      case SILInstructionKind::AddressToPointerInst:
      case SILInstructionKind::StoreWeakInst:
      case SILInstructionKind::MarkDependenceInst:
        return true;
        
      case SILInstructionKind::ApplyInst:
      case SILInstructionKind::BeginApplyInst:
      case SILInstructionKind::PartialApplyInst:
      case SILInstructionKind::TryApplyInst:
        return externalFunctionArgIsModified(I, Op);
        
      default:
        return false;
    }
  }
  
  /// Stage values for def-use traversal
  void search(SILInstruction *I, Operand *Op) {
    switch (I->getKind()) {
      case SILInstructionKind::BeginBorrowInst:
      case SILInstructionKind::ConvertEscapeToNoEscapeInst:
      case SILInstructionKind::CopyValueInst:
      case SILInstructionKind::EnumInst:
      case SILInstructionKind::MarkUninitializedInst:
      case SILInstructionKind::ProjectBoxInst:
      case SILInstructionKind::BeginAccessInst:
        appendValues(I);
        break;
        
      case SILInstructionKind::ApplyInst:
      case SILInstructionKind::BeginApplyInst:
      case SILInstructionKind::PartialApplyInst:
      case SILInstructionKind::TryApplyInst:
        appendValues(I);
        if (SILValue Arg = getFunctionArgInst(I, Op)) {
          appendValue(Arg);
        }
        break;
      default:
        break;
    }
  }
  
public:
  
  /// Perform the traversal and return if a modify was found.
  bool didModify() {
    
    if (auto DVO = getDebugVarValue(EntryInst)) {
      appendValue(DVO);
    } else if (auto DVAO = getDebugVarAddrValue(EntryInst)) {
      appendValue(DVAO);
    }
    
    while (!Worklist.empty()) {
      
      auto Value = Worklist.pop_back_val();
      
      // Return Trivial Cases
      for (Operand *Use : Value->getUses()) {
        if (SILInstruction *User = Use->getUser()) {
          if (isDirectModier(User, Use)) {
            return true;
          }
        }
      }
      
      // Traverse def-use
      for (Operand *Use : Value->getUses()) {
        if (SILInstruction *User = Use->getUser()) {
          search(User, Use);
        }
      }
    }
    return false;
  }
};

/// Traverses through the def-use of an instruction representing a var
/// declaration to find reads of the value.
class VariableReadTraverser {
  
  /// The instruction to search
  SILInstruction *EntryInst;
  
  /// A list of values staged for traversal
  SmallVector<SILValue, 32> Worklist;
  
  /// All values reached in the traversal
  llvm::SetVector<SILValue> Seen;
  
  /// A list of values staged for direct checks and traversal
  llvm::SetVector<SILInstruction *> DirectCheckWorklist;
  
public:
  VariableReadTraverser(SILInstruction *EntryInst) : EntryInst(EntryInst) {
    
    assert(getVarDecl(EntryInst) &&
           "Expected an instruction belonging to a VarDecl");
    
    appendValues(EntryInst);
  }
  
private:
  
  /// Stage instruction results for traversal
  void appendValues(SILInstruction *I) {
    for (SILValue Value : I->getResults()) {
      appendValue(Value);
    }
  }
  
  /// Stage a value for traversal
  void appendValue(SILValue Value) {
    if (!Seen.count(Value)) {
      Worklist.push_back(Value);
      Seen.insert(Value);
    }
  }
  
  /// Check the AST to determine if a var declaration is directly assigned
  /// to the other var declaration.
  ///
  /// \p let vd = 12
  /// \p let assignee = vd
  ///
  static bool isAssignedToVarDecl(VarDecl *VD, VarDecl *Assignee) {
    if (Assignee->isDebuggerVar()) {
      return false;
    }
    if (auto Init = Assignee->getParentInitializer()) {
      if (auto DRE = dyn_cast<DeclRefExpr>(Init)) {
        if (auto Value = DRE->getDecl()) {
          return Value->getBaseIdentifier() == VD->getName();
        }
        return false;
      }
    }
    return false;
  }
  
  /// Returns whether an ApplySite contains a closure parameter that has at
  /// least one argument.
  static bool hasClosureWithArguments(ApplySite AS) {
    for (auto &Op : AS.getArgumentOperands()) {
      auto Value = Op.get();
      if (auto FnType = Value->getType().getAs<SILFunctionType>()) {
        return FnType->getNumParameters();
      }
    }
    return false;
  }
  
  /// Determine if external SIL function reads a value. Returns true if the
  /// parameter is not an inout or if the function contains a closure parameter
  /// that has at least one argument.
  bool functionArgIsRead(SILInstruction *Apply, Operand *Op) {
    
    if (!Op) {
      return false;
    }
    
    ApplySite AS;
    SILFunction *Fn;
    
    if (auto AI = dyn_cast<ApplyInst>(Apply)) {
      AS = ApplySite(AI);
      Fn = AI->getReferencedFunctionOrNull();
    } else if (auto PAI = dyn_cast<PartialApplyInst>(Apply)) {
      AS = ApplySite(PAI);
      Fn = PAI->getReferencedFunctionOrNull();
    } else if (auto TAI = dyn_cast<TryApplyInst>(Apply)) {
      AS = ApplySite(TAI);
      Fn = TAI->getReferencedFunctionOrNull();
    } else if (auto BAI = dyn_cast<BeginApplyInst>(Apply)) {
      AS = ApplySite(BAI);
      Fn = BAI->getReferencedFunctionOrNull();
    }
    
    if (AS && Fn) {
      
      // If a local function captures the value, this isn't a direct read.
      // We will traverse through the function later to determine if it
      // the variable is read.
      if (!Fn->isExternalDeclaration()) {
        if (auto FnDeclCtx = Fn->getDeclContext()) {
          if (auto FnDecl = dyn_cast<FuncDecl>(FnDeclCtx)) {
            SmallVector<CapturedValue, 2> LocalCaptures;
            FnDecl->getCaptureInfo().getLocalCaptures(LocalCaptures);
            auto VD = getVarDecl(EntryInst);
            for (auto Capture : LocalCaptures) {
              auto Value = Capture.getDecl();  
              if (Value->getBaseIdentifier() == VD->getName()) {
                return false;
              }
            }
          }
        }
      }
      if ((Op->getUser() == Apply) && AS.isArgumentOperand(*Op)) {
        
        // If the parameter is not an inout, trust that the function reads
        // the argument.
        if (!argIsInout(AS.getArgumentConvention(*Op))) {
          return true;
        }
        
        // If any function argument is a closure, the inout value may be
        // read to produce parameters for the closure. We can not guarantee this,
        // so we may generate a false positive.
        if (hasClosureWithArguments(AS)) {
          return true;
        }
      }
    }
    return false;
  }
  
  /// Determines if an Apply instruction terminates the traversal as a read of
  /// the entry instruction.
  bool applyIsDirectRead(SILInstruction *I, Operand *Op, SILValue Value) {
    
    if (auto *OpInst =
        I->getAllOperands()[0].get().getDefiningInstruction()) {
      if (isa<WitnessMethodInst>(OpInst)) {
        return true;
      } else if (isa<ClassMethodInst>(OpInst)) {
        return true;
      } else if (isa<BuiltinInst>(OpInst)) {
        return true;
      } else if (isa<BeginBorrowInst>(OpInst)) {
        // FIXME: check if it is operand index 1?
        //          if (I->getAllOperands()[])
        return true;
      } else if (isa<ObjCMethodInst>(OpInst)) {
        // FIXME: Can we know if it is read or inout?
        return true;
      } 
    }
    
    if (auto Operand = I->getAllOperands()[0].get()) {
      if (Operand == Value) {
        // This variable is a closure and is being called.
        return true;
      } else if (isa<SILFunctionArgument>(Operand)) {
        // This is calling a closure passed in as a function argument
        // A read can not be guaranteed in runtime, but can be assumed
        return true;
      }
    }
    
    // If this function is declared externally, and we can
    // inspect argument conventions.
    return functionArgIsRead(I, Op);
  }
  
  /// Finds the corresponding SILArgument for a parameter in a basic block
  static SILArgument *getBasicBlockArgInst(BranchInst *BI, SILValue Param) {
    
    // Get index of instruction in operand list.
    unsigned argIndex = 0;
    if (BI->getArgs().size() < 1) {
      return nullptr;
    }
    
    // Find the SILValue in list of Operands
    if (auto Value = BI->getArgs()[argIndex]) {
      while (Value != Param) {
        ++argIndex;
        if (argIndex == BI->getArgs().size()) {
          return nullptr;
        }
        Value = BI->getArgs()[argIndex];
      }
    } else {
      return nullptr;
    }
    
    if (auto *bb = BI->getDestBB()) {
      if (SILArgument *Arg = bb->getArgument(argIndex)) {
        return Arg;
      }
    }
    return nullptr;
  }

  /// Determines if a store instruction terminates the traversal as a read of
  /// the entry instruction.
  bool storeInstIsRead(SILInstruction *I, SILValue Value) {
    auto SI = dyn_cast<StoreInst>(I);
    if (SI->getSrc() != Value) {
      return false;
    }
    
    auto DI = SI->getDest().getDefiningInstruction();
    
    // FIXME: Assuming this is a store into inout
    if (!DI) {
      return true;
    }
    
    // The destination of this store is a Var Declaration.
    // Because we are tracking the use of a value, which may be referenced
    // by multiple Var Declarations, check in the AST if the assignment
    // specifically uses the current Var Declaration.
    if (auto *DestVD = getVarDecl(DI)) {
      auto VD = getVarDecl(EntryInst);
      return isAssignedToVarDecl(VD, DestVD);
    }
    
    return true;
  }
  
  /// Checks if a SILInstruction belongs to any block accessible by the SILBasicBlock
  bool inAccessibleBlock(SILInstruction *I) {
    SmallVector<SILBasicBlock *, 32> BlockList;
    llvm::SetVector<SILBasicBlock *> SeenBlocks;
    BlockList.push_back(EntryInst->getParent());
    SeenBlocks.insert(EntryInst->getParent());
    while (!BlockList.empty()) {
      auto *BB = BlockList.pop_back_val();
      
      // Check if the instruction is in this block
      if (I->getParent() == BB)
        return true;
      
      // Search for more branches
      auto i = BB->begin(), e = BB->end();
      while (i != e) {
        if (auto BI = dyn_cast<BranchInst>(&*i)) {
          if (!SeenBlocks.count(BI->getDestBB())) {
            SeenBlocks.insert(BI->getDestBB());
            BlockList.push_back(BI->getDestBB());
          }
        } else if (auto CBI = dyn_cast<CondBranchInst>(&*i)) {
          if (!SeenBlocks.count(CBI->getTrueBB())) {
            SeenBlocks.insert(CBI->getTrueBB());
            BlockList.push_back(CBI->getTrueBB());
          }
          if (!SeenBlocks.count(CBI->getFalseBB())) {
            SeenBlocks.insert(CBI->getFalseBB());
            BlockList.push_back(CBI->getFalseBB());
          }
        } else if (auto SEI = dyn_cast<SwitchEnumInst>(&*i)) {
          for (unsigned c = 0; c < SEI->getNumCases(); ++c) {
            auto *Case = SEI->getCase(c).second;
            if (!SeenBlocks.count(Case)) {
              SeenBlocks.insert(Case);
              BlockList.push_back(Case);
            }         
          }
        } else if (auto SEAI = dyn_cast<SwitchEnumAddrInst>(&*i)) {
          for (unsigned c = 0; c < SEAI->getNumCases(); ++c) {
            auto *Case = SEAI->getCase(c).second;
            if (!SeenBlocks.count(Case)) {
              SeenBlocks.insert(Case);
              BlockList.push_back(Case);
            }         
          }
        } else if (auto SVI = dyn_cast<SwitchValueInst>(&*i)) {
          for (unsigned c = 0; c < SVI->getNumCases(); ++c) {
            auto *Case = SVI->getCase(c).second;
            if (!SeenBlocks.count(Case)) {
              SeenBlocks.insert(Case);
              BlockList.push_back(Case);
            }
          }
        } else if (auto DMBI = dyn_cast<DynamicMethodBranchInst>(&*i)) {
          if (!SeenBlocks.count(DMBI->getHasMethodBB())) {
            SeenBlocks.insert(DMBI->getHasMethodBB());
            BlockList.push_back(DMBI->getHasMethodBB());
          }
          if (!SeenBlocks.count(DMBI->getNoMethodBB())) {
            SeenBlocks.insert(DMBI->getNoMethodBB());
            BlockList.push_back(DMBI->getNoMethodBB());
          }
        } else if (auto CCBI = dyn_cast<CheckedCastBranchInst>(&*i)) {
          if (!SeenBlocks.count(CCBI->getSuccessBB())) {
            SeenBlocks.insert(CCBI->getSuccessBB());
            BlockList.push_back(CCBI->getSuccessBB());
          }
          if (!SeenBlocks.count(CCBI->getFailureBB())) {
            SeenBlocks.insert(CCBI->getFailureBB());
            BlockList.push_back(CCBI->getFailureBB());
          }
        } else if (auto CCVBI = dyn_cast<CheckedCastValueBranchInst>(&*i)) {
          if (!SeenBlocks.count(CCVBI->getSuccessBB())) {
            SeenBlocks.insert(CCVBI->getSuccessBB());
            BlockList.push_back(CCVBI->getSuccessBB());
          }
          if (!SeenBlocks.count(CCVBI->getFailureBB())) {
            SeenBlocks.insert(CCVBI->getFailureBB());
            BlockList.push_back(CCVBI->getFailureBB());
          }
        } else if (auto CCABI = dyn_cast<CheckedCastAddrBranchInst>(&*i)) {
          if (!SeenBlocks.count(CCABI->getSuccessBB())) {
            SeenBlocks.insert(CCABI->getSuccessBB());
            BlockList.push_back(CCABI->getSuccessBB());
          }
          if (!SeenBlocks.count(CCABI->getFailureBB())) {
            SeenBlocks.insert(CCABI->getFailureBB());
            BlockList.push_back(CCABI->getFailureBB());
          }
        } else if (auto TAI = dyn_cast<TryApplyInst>(&*i)) {
          if (!SeenBlocks.count(TAI->getNormalBB())) {
            SeenBlocks.insert(TAI->getNormalBB());
            BlockList.push_back(TAI->getNormalBB());
          }
          if (!SeenBlocks.count(TAI->getErrorBB())) {
            SeenBlocks.insert(TAI->getErrorBB());
            BlockList.push_back(TAI->getErrorBB());
          }
        } else if (auto AACI = dyn_cast<AwaitAsyncContinuationInst>(&*i)) {
          if (!SeenBlocks.count(AACI->getResumeBB())) {
            SeenBlocks.insert(AACI->getResumeBB());
            BlockList.push_back(AACI->getResumeBB());
          }
          if (!SeenBlocks.count(AACI->getErrorBB())) {
            SeenBlocks.insert(AACI->getErrorBB());
            BlockList.push_back(AACI->getErrorBB());
          }
        }
        ++i;
      }
    }
    return false;
  }
  
  /// Determines whether a reading instruction may be ignored. This can happen
  /// if the instruction occurs in a block inaccessible by the entry instruction.
  bool notIgnored(SILInstruction *I) {
    if (auto DebugVD = getVarDecl(EntryInst)) {
      if (auto ParentStmt = DebugVD->getRecursiveParentPatternStmt()) {
        if (isa<CaseStmt>(ParentStmt)) {
          return inAccessibleBlock(I);
        }
      }
    }
    return true;
  }
  
  /// Determines if an instruction terminates the traversal as a read of
  /// the entry instruction.
  bool isDirectRead(SILInstruction *I, Operand *Op, SILValue Value) {
    
    if (isSelfParam(I)) {
      return true;
    }
    
    switch (I->getKind()) {
      
      // Returning this value is a direct read
      case SILInstructionKind::ReturnInst:
      
      // FIXME: Is this true?
      // Builtin Instructions found in the standard library
      // are responsible for logging and other utilities.
      // count this as a read.
      case SILInstructionKind::BuiltinInst:  
        
      // This value is read in an if-statement
      case SILInstructionKind::CondBranchInst:
        
      // This value is thrown, and therefore read
      case SILInstructionKind::ThrowInst:
      
      // FIXME: Should this be handled like SwitchEnumInst?
      case SILInstructionKind::SwitchEnumAddrInst:
      
      // FIXME: why?
      // FIXME: Should we search?
      case SILInstructionKind::StoreBorrowInst:
        
      // FIXME: why?
      // FIXME: Should we search?
      case SILInstructionKind::OpenExistentialAddrInst:
      
      // These instructions may not be the actual read of the variable, but
      // the def-use always terminates in a read for these instructions, so 
      // end the traversal early
      case SILInstructionKind::CopyValueInst:
      case SILInstructionKind::PointerToAddressInst:
        return true;
        
      case SILInstructionKind::StoreInst:
        return storeInstIsRead(I, Value);
      case SILInstructionKind::AssignInst:
        return dyn_cast<AssignInst>(I)->getSrc() == Value;
      case SILInstructionKind::CopyAddrInst:
        return dyn_cast<CopyAddrInst>(I)->getSrc() == Value;
        
        // FIXME: Check for other binding assignments?
      case SILInstructionKind::SwitchValueInst:
      case SILInstructionKind::SwitchEnumInst: 
        return true;
        
      case SILInstructionKind::DebugValueInst:
        if (auto *DebugVD = getVarDecl(I)) {
          auto VD = getVarDecl(EntryInst);
          
          // This value is assigned to a capture list.
          if (DebugVD->isCaptureList() && DebugVD != VD) {
            return true;
          }
          
          // The value described by a Var Declaration may be assigned to multiple
          // Var Declarations, but this does not always mean the Var Declaration
          // is read by others, so we must explicitly check the DeclRefExpr in
          // the AST.
          return isAssignedToVarDecl(VD, DebugVD);
        }
        return true;
        
      case SILInstructionKind::BeginAccessInst:
        return (dyn_cast<BeginAccessInst>(I)->getAccessKind() == SILAccessKind::Read);
      case SILInstructionKind::BeginUnpairedAccessInst:
        return (dyn_cast<BeginUnpairedAccessInst>(I)->getAccessKind() == SILAccessKind::Read);
        
      case SILInstructionKind::ApplyInst:
      case SILInstructionKind::BeginApplyInst:
      case SILInstructionKind::PartialApplyInst:
      case SILInstructionKind::TryApplyInst:
        return applyIsDirectRead(I, Op, Value);
      default:
        return false;
    }
  }
  
  /// Stage values for def-use traversal
  void search(SILInstruction *I, Operand *Op, SILValue Value) {
    switch (I->getKind()) {
        
      // FIXME: should we only look for direct uses here? Instead of traversing
      case SILInstructionKind::InitExistentialAddrInst: {
        if (auto DI = I->getAllOperands()[0].get().getDefiningInstruction()) {
          DirectCheckWorklist.insert(DI);
        }
        break;
      }
        
      // This case occurs if the store was not determined as a direct read.
      // We will trace the store destination for use of the value, such as
      // an Apply Instruction.
      case SILInstructionKind::StoreInst: {
        auto SI = dyn_cast<StoreInst>(I);
        if (SI->getSrc() != Value) {
          break;
        }
        if (auto DI = SI->getDest().getDefiningInstruction()) {
          DirectCheckWorklist.insert(DI);
          appendValues(DI);
        }
        break;
      }
        
      case SILInstructionKind::BranchInst:
        if (auto *Arg = getBasicBlockArgInst(dyn_cast<BranchInst>(I), Value)) {
          appendValue(Arg);
        }
        break;
        
      case SILInstructionKind::ApplyInst:
      case SILInstructionKind::BeginApplyInst:
      case SILInstructionKind::PartialApplyInst:
      case SILInstructionKind::TryApplyInst: {
        
        appendValues(I);
        
        // This function is defined locally, and we can step in to
        // track variable usage
        if (SILArgument *Arg = getFunctionArgInst(I, Op)) {
          appendValue(Arg);
        }
        break;
      }
        
      default:
        appendValues(I);
    }
  }
  
public:
  
  /// Perform the traversal and return if a read was found.
  bool didRead() {
    
    if (isSelfParam(EntryInst)) {
      return true;
    } else if (auto DVO = getDebugVarValue(EntryInst)) {
      appendValue(DVO);
    } else if (auto DVAO = getDebugVarAddrValue(EntryInst)) {
      appendValue(DVAO);
    }
    
    while (!Worklist.empty()) {
      
      auto Value = Worklist.pop_back_val();
      
      // Return Trivial Cases
      if (auto Arg = dyn_cast<SILFunctionArgument>(Value)) {
        if (Arg->isSelf()) {
          return true;
        }
      }
      
      for (Operand *Use : Value->getUses()) {
        if (SILInstruction *User = Use->getUser()) {
          if (isDirectRead(User, Use, Value) &&
              notIgnored(User)) {
            return true;
          }
        }
      }
      
      // Traverse def-use
      for (Operand *Use : Value->getUses()) {
        if (SILInstruction *User = Use->getUser()) {
          search(User, Use, Value);
          
          // Check instructions staged for direct checks
          for (SILInstruction *Inst : DirectCheckWorklist) {
            DirectCheckWorklist.remove(Inst);
            if (isDirectRead(Inst, Use, Value) &&
                notIgnored(User)) {
              return true;
            }
            search(Inst, Use, Value);
          }
        }
      }
    }
    return false;
  }
};

/// Traverses through a setter function to find any references to the variable's
/// getter function. Use this traversal if the setter function does not use the
/// 'newValue' parameter to generate diagnostics.
class SetterReadsGetterTraverser {
  
  /// The setter function to perform the search
  SILFunction *SetterFn;
  
  /// The instruction referencing the getter function if found
  SILInstruction *GetterInst;
  
public:
  SetterReadsGetterTraverser(SILFunction *SetterFn) : SetterFn(SetterFn) { }
  
  /// Returns the instruction referencing the getter function or \c nullptr
  /// if not found. Only use this after performing the traversal
  /// with \c findGetterAccess()
  SILInstruction *getGetterInst() {
    return GetterInst;
  }
  
  /// Performs the traversal
  bool findGetterAccess() {
    for (auto &bb : *SetterFn) {
      auto i = bb.begin(), e = bb.end();
      while (i != e) {
        SILInstruction *I = &*i;
        
        if (auto FRI = dyn_cast<FunctionRefInst>(I)) {
          if (auto Fn = FRI->getReferencedFunctionOrNull()) {
            if (auto DC = Fn->getDeclContext()) {
              if (auto AD = dyn_cast_or_null<AccessorDecl>(DC)) {
                if (AD->getAccessorKind() == AccessorKind::Get) {
                  GetterInst = FRI;
                  return true;
                }
              }
            }
          }
        }
        
        if (auto CMI = dyn_cast<ClassMethodInst>(I)) {
          if (auto Member = CMI->getMember()) {
            if (auto AD = dyn_cast<AccessorDecl>(Member.getDecl())) {
              if (AD->getAccessorKind() == AccessorKind::Get) {
                GetterInst = CMI;
                return true;
              }
            }
          }
        }
        
        ++i;
      }
    }
    return false;
  }
  
  void diagnoseGetterUse(VarDecl *var) {
    
    auto &diags = SetterFn->getModule().getASTContext().Diags;
    
  diags.diagnose(GetterInst->getLoc().getSourceLoc(),
                 diag::unused_setter_parameter,
                 var->getName());
    diags.diagnose(GetterInst->getLoc().getSourceLoc(),
                   diag::fixit_for_unused_setter_parameter,
                   var->getName())
    .fixItReplace(GetterInst->getLoc().getSourceRange(),
                  var->getName().str());
  }
};

//===----------------------------------------------------------------------===//
//                        MARK: Traverser Interfaces
//===----------------------------------------------------------------------===//

/// Look for a \c PartialApplyInst before the next capture reference
static bool didReadCapture(SILInstruction *I) {
  
  auto VD = getVarDecl(I);
  assert(isa<DebugValueInst>(I) && "Expected a DebugValueInst");
  assert(VD->isCaptureList() && "Expected a Capture List");
  auto CapturedValue = dyn_cast<DebugValueInst>(I)->getOperand();
  SILInstruction *NextCapture = nullptr;
  SILBasicBlock *BB = I->getParent();
  auto i = BB->begin(), e = BB->end();
  
  // Find the next capture reference
  bool foundI = false;
  while (i != e) {
    if (&*i == I) {
      foundI = true;
    }
    
    if (!foundI || !isa<DebugValueInst>(&*i)) {
      ++i;
      continue;
    }
    if (auto OtherVD = getVarDecl(&*i)) {
      if (OtherVD->isCaptureList() &&
          OtherVD != VD &&
          getDebugVarValue(&*i) == CapturedValue) {
        NextCapture = &*i;
        break;
      }
    }
    ++i;
    continue;
  }
  
  // Search for a Partial Apply Use
  for (auto Use : CapturedValue->getUses()) {
    auto User = Use->getUser();
    if (isa<PartialApplyInst>(User)) {
      i = BB->begin();
      foundI = false;
      
      // Check the Partial Apply is after this capture reference and before
      // the next capture reference
      while (i != e) {
        if (&*i == I) {
          foundI = true;
        } else if (&*i == NextCapture) {
          break;
        } else if (foundI && &*i == User) {
          return true;
        }
        ++i;
      }
    }
  }
  return false;
}

static bool didModify(SILInstruction *I) {
  auto VMT = VariableModifyTraverser(I);
  return VMT.didModify();
}

static bool didRead(SILInstruction *I) {
  auto VD = getVarDecl(I);
  assert(VD && "Expected an instruction belonging to a VarDecl");
  if (VD->isCaptureList() && isa<DebugValueInst>(I)) {
    return didReadCapture(I);
  }
  auto VRT = VariableReadTraverser(I);
  return VRT.didRead();
}

static void searchSetterParam(SILInstruction *I) {
  auto VD = getVarDecl(I);
  assert(VD && "Expected an instruction belonging to a VarDecl");
  auto VRT = VariableReadTraverser(I);
  if (!VRT.didRead()) {
    if (auto Fn = I->getFunction()) {
      auto SRGT = SetterReadsGetterTraverser(Fn);
      if (SRGT.findGetterAccess()) {
        SRGT.diagnoseGetterUse(VD);
      }
    }
  }
}

/// Get the first \c DebugValueInst to reference the value
static VarDecl *getInitialReference(SILInstruction *I) {
  auto DVI = dyn_cast<DebugValueInst>(I);
  if (!DVI) {
    return nullptr;
  }
  
  if (auto DebugVD = getVarDecl(I)) {
    if (DebugVD->isCaptureList()) {
      return nullptr;
    }
    if (auto ParentStmt = DebugVD->getRecursiveParentPatternStmt()) {
      if (isa<CaseStmt>(ParentStmt)) {
        return nullptr;
      }
    }
  }
  
  auto ReferencedValue = DVI->getAllOperands()[0].get();
  
  DebugValueInst *InitialRef = DVI;
  for (auto Use : ReferencedValue->getUses()) {
    auto UserDVI = dyn_cast<DebugValueInst>(Use->getUser()); 
    if (!DVI) {
      continue;
    }
    if (instructionPrecedes(UserDVI, DVI, I->getParent())) {
      InitialRef = UserDVI;
    }
  }
  
  if (InitialRef == DVI) {
    return nullptr;
  }
  
  if (auto VD = getVarDecl(InitialRef)) {
    return VD;
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
//                         MARK: Diagostics Generator
//===----------------------------------------------------------------------===//

class UsageDiagnosticsGenerator {
  DiagnosticEngine &diags;
  
public:
  
  UsageDiagnosticsGenerator(ASTContext &Context) : diags(Context.Diags) {}
  
  void diagnoseDuplicateReference(VarDecl *VD, VarDecl *IVD) {
    diags.diagnoseWithNotes(
      diags.diagnose(VD->getLoc(),       
                     diag::immutable_value_duplicate_reference,
                     VD->getName(), IVD->getName()),
                     [&]() {
      // Emit a note to swap to mark the initial reference
      diags.diagnose(IVD->getLoc(), diag::value_initial_reference);
    
    });
  }
  
  void diagnoseUnreadUnmodified(VarDecl *var) {
    
    // If the source of the VarDecl is a trivial PatternBinding with only a
    // single binding, rewrite the whole thing into an assignment.
    //    let x = foo()
    //  ->
    //    _ = foo()
    if (auto *pbd = var->getParentPatternBinding()) {
      if (pbd->getSingleVar() == var && pbd->getInit(0) != nullptr &&
          !isa<TypedPattern>(pbd->getPattern(0))) {
        unsigned varKind = var->isLet();
        SourceRange replaceRange(pbd->getStartLoc(),
                                 pbd->getPattern(0)->getEndLoc());
        diags.diagnose(var->getLoc(), diag::pbd_never_used,
                       var->getName(), varKind)
        .fixItReplace(replaceRange, "_");
        return;
      }
    }
    
    // If the variable is defined in a pattern in an if/while/guard statement,
    // see if we can produce a tuned fixit.  When we have something like:
    //
    //    if let x = <expr> {
    //
    // we prefer to rewrite it to:
    //
    //    if <expr> != nil {
    if (auto ps = var->getParentPatternStmt()) {
      if (auto SC = dyn_cast<LabeledConditionalStmt>(ps)) {
        // We only handle the "if let" case right now, since it is vastly the
        // most common situation that people run into.
        if (SC->getCond().size() == 1) {
          auto pattern = SC->getCond()[0].getPattern();
          if (auto OSP = dyn_cast<OptionalSomePattern>(pattern)) {
            if (auto LP = dyn_cast<BindingPattern>(OSP->getSubPattern())) {
              if (isa<NamedPattern>(LP->getSubPattern())) {
                auto initExpr = SC->getCond()[0].getInitializer();
                if (initExpr->getStartLoc().isValid()) {
                  unsigned noParens = initExpr->canAppendPostfixExpression();
                  
                  // If the subexpr is an "as?" cast, we can rewrite it to
                  // be an "is" test.
                  bool isIsTest = false;
                  if (isa<ConditionalCheckedCastExpr>(initExpr) &&
                      !initExpr->isImplicit()) {
                    noParens = isIsTest = true;
                  }
                  
                  auto diagIF = diags.diagnose(var->getLoc(),
                                               diag::pbd_never_used_stmtcond,
                                               var->getName());
                  auto introducerLoc = SC->getCond()[0].getIntroducerLoc();
                  diagIF.fixItReplaceChars(introducerLoc,
                                           initExpr->getStartLoc(),
                                           &"("[noParens]);
                  
                  if (isIsTest) {
                    // If this was an "x as? T" check, rewrite it to "x is T".
                    auto CCE = cast<ConditionalCheckedCastExpr>(initExpr);
                    diagIF.fixItReplace(SourceRange(CCE->getLoc(),
                                                    CCE->getQuestionLoc()),
                                        "is");
                  } else {
                    diagIF.fixItInsertAfter(initExpr->getEndLoc(),
                                            &") != nil"[noParens]);
                  }
                  return;
                }
              }
            }
          }
        }
      }
      
    }
    
    // If the variable is defined in a pattern that isn't one of the usual
    // conditional statements, try to detect and rewrite "simple" binding
    // patterns:
    //    case .pattern(let x):
    //  ->
    //    case .pattern(_):
    if (auto *pattern = var->getParentPattern()) {
      BindingPattern *foundVP = nullptr;
      pattern->forEachNode([&](Pattern *P) {
        if (auto *VP = dyn_cast<BindingPattern>(P))
          if (VP->getSingleVar() == var)
            foundVP = VP;
      });
      
      if (foundVP) {
        unsigned varKind = var->isLet();
        diags.diagnose(var->getLoc(), diag::variable_never_used,
                       var->getName(), varKind)
        .fixItReplace(foundVP->getSourceRange(), "_");
        return;
      }
    }
    
    
    // If this is a member in a capture list, just say it is unused.  We could
    // produce a fixit hint with a parent map, but this is a lot of effort for
    // a narrow case.
    if (var->isCaptureList()) {
      diags.diagnose(var->getLoc(),
                     diag::capture_never_used,
                     var->getName());
      return;
    }
    
    // FIXME: Never run?
    // Otherwise, this is something more complex, perhaps
    //    let (a,b) = foo()
//    if (var->isLet() && false) {
//      diags.diagnose(var->getLoc(),
//                             diag::immutable_value_never_used_but_assigned,
//                             var->getName());
//    }
    
    unsigned varKind = var->isLet();
    // Just rewrite the one variable with a _.
    diags.diagnose(var->getLoc(), diag::variable_never_used,
                   var->getName(), varKind)
    .fixItReplace(var->getLoc(), "_");
  }
  
  void diagnoseUnreadModified(VarDecl *var) {
    
    //    diagnoseUnreadUnmodified(var, true);
    // If this is a variable that was only written to, emit a warning.
    diags.diagnose(var->getLoc(),
                   diag::variable_never_read,
                   var->getName());
  }
  
  void diagnoseReadUnmodified(VarDecl *var) {
    
    if (var->isLet()) {
      return;
    }
    
    SourceLoc FixItLoc;
    
    // Try to find the location of the 'var' so we can produce a fixit.  If
    // this is a simple PatternBinding, use its location.
    if (auto *PBD = var->getParentPatternBinding()) {
      if (PBD->getSingleVar() == var)
        FixItLoc = PBD->getLoc();
    } else if (auto *pattern = var->getParentPattern()) {
      BindingPattern *foundVP = nullptr;
      pattern->forEachNode([&](Pattern *P) {
        if (auto *VP = dyn_cast<BindingPattern>(P))
          if (VP->getSingleVar() == var)
            foundVP = VP;
      });
      
      if (foundVP && !foundVP->isLet())
        FixItLoc = foundVP->getLoc();
    }
    
    // If this is a parameter explicitly marked 'var', remove it.
    if (FixItLoc.isInvalid()) {
      diags.diagnose(var->getLoc(), diag::variable_never_mutated,
                     var->getName(), true);
    } else {
      bool suggestLet = true;
      if (auto *stmt = var->getRecursiveParentPatternStmt()) {
        // Don't try to suggest 'var' -> 'let' conversion
        // in case of 'for' loop because it's an implicitly
        // immutable context.
        suggestLet = !isa<ForEachStmt>(stmt);
      }
      
      auto diag = diags.diagnose(var->getLoc(),
                                 diag::variable_never_mutated,
                                 var->getName(), suggestLet);
      
      if (suggestLet) {
        // FIXME: Not emitting?
        diag.fixItReplace(FixItLoc, "let");
      } else {
        diag.fixItRemove(FixItLoc);
      }
    }
  }
};


//===----------------------------------------------------------------------===//
//                           MARK: Top Level Driver
//===----------------------------------------------------------------------===//

namespace {

/// Perform usage analysis of instructions representing variables
class DiagnoseVarUsage : public SILFunctionTransform {
  
  /// Check that variables are read and mutated
  /// Generate diagnostics for unused, unread, or unmutated variables
  static void checkVarUsage(SILFunction &Fn) {
    LLVM_DEBUG(llvm::dbgs() << "*** Diagnose Var Usage visiting function: "
               <<  Fn.getName() << "\n");
    
    SILModule &M = Fn.getModule();
    
    // Create collections for each diagnostic pipeline
    llvm::SetVector<VarDecl *> VarAll;
    llvm::SetVector<VarDecl *> VarUnreadUnmodified;
    llvm::SetVector<VarDecl *> VarReadUnmodified;
    llvm::SetVector<VarDecl *> VarUnreadModified;
    llvm::SetVector<VarDecl *> VarReadModified;
    llvm::MapVector<VarDecl *, VarDecl *> VarDuplicateReference;
    
    for (auto &bb : Fn) {
      auto i = bb.begin(), e = bb.end();
      while (i != e) {
        
        SILInstruction *Inst = getEntryInst(&*i);
        if (!Inst) {
          ++i;
          continue;
        }
        
        VarDecl *VD = getVarDecl(Inst);
        
        if (isSetterParam(Inst)) {
          searchSetterParam(Inst);
          ++i;
          continue;
        }
        
        if (!VarAll.count(VD)) {
          VarAll.insert(VD);
        }
        
        // Since this diagnostic takes priority, if an instruction from the declaration has
        // already been marked as a duplicate reference, skip recheck.
        if (VarDuplicateReference.lookup(VD)) {
          ++i;
          continue;
        }
        
        // If an instruction from the declaration has
        // already been marked as read and modified, skip recheck.
        if (VarReadModified.count(VD)) {
          ++i;
          continue;
        }
        
        // Pop read and unmodified declarations to consider promotion to read
        // and modified.
        // Promote declarations if the instruction is modified.
        if (VarReadUnmodified.count(VD)) {
          if (didModify(Inst)) {
            VarReadUnmodified.remove(VD);
            VarReadModified.insert(VD);
          }
          ++i;
          continue;
        }
        
        // Pop unread and modified declarations to consider promotion to read
        // and modified.
        if (VarUnreadModified.count(VD)) {
          if (didRead(Inst)) {
            VarUnreadModified.remove(VD);
            VarReadModified.insert(VD);
          }
          ++i;
          continue;
        }
        
        // Pop unread and unmodified declarations to consider any promotion.
        if (VarUnreadUnmodified.count(VD)) {
          VarUnreadUnmodified.remove(VD);
        }
        
        // Promote new declarations and unread and unmodified declarations
        bool DidRead = didRead(Inst);
        bool DidModify = didModify(Inst);
        if (auto InitialVD = getInitialReference(Inst)) {
          VarDuplicateReference.insert(std::make_pair(VD, InitialVD));
        } else if (!DidRead && !DidModify) {
          VarUnreadUnmodified.insert(VD);
        } else if (DidRead && !DidModify) {
          VarReadUnmodified.insert(VD);
        } else if (!DidRead && DidModify) {
          VarUnreadModified.insert(VD);
        } else {
          VarReadModified.insert(VD);
        }
        ++i;
        continue;
      }
    }
    
    // Tuple Promotion
    //
    // When a var declaration belongs to a tuple, and a sibling is mutated,
    // don't diagnose that unmodified siblings be declared as 'lets'. Promote
    // the use of unmodified siblings as modified.
    // 
    //   var (a, b) = (1, 2) <--- don't suggest b as a 'let'
    //   a += 1
    //
    llvm::SetVector<VarDecl *> Promoted;
    for (VarDecl *VD : VarAll) {
      
      // Skip promotion of tuple siblings if already promoted.
      if (Promoted.count(VD)) {
        continue;
      }
      
      // Only modified variables are elligible to promote siblings.
      if (VarReadUnmodified.count(VD) || VarUnreadUnmodified.count(VD)) {
        continue;
      }
      
      // Perform promotions
      if (auto *Pattern = VD->getParentPattern()) {
        auto Kind = Pattern->getKind();
        if (Kind == PatternKind::Typed) {
          Kind = dyn_cast<TypedPattern>(Pattern)->getSubPattern()->getKind();
        }
        if (Kind == PatternKind::Tuple) {
          Pattern->forEachVariable([&](VarDecl *SiblingVD) {
            if (VarUnreadUnmodified.count(SiblingVD)) {
              // FIXME: does this make sense? or should this not promote?
              VarUnreadUnmodified.remove(SiblingVD);
              VarUnreadModified.insert(SiblingVD);
              Promoted.insert(SiblingVD);
            } else if (VarReadUnmodified.count(SiblingVD)) {
              VarReadUnmodified.remove(SiblingVD);
              VarReadModified.insert(SiblingVD);
              Promoted.insert(SiblingVD);
            }
          });
        }
      }
    }
    
    // Run Diagnostics
    
    UsageDiagnosticsGenerator UDG(M.getASTContext());
    for (VarDecl *VD : VarAll) {
      if (auto IVD = VarDuplicateReference.lookup(VD)) {
        UDG.diagnoseDuplicateReference(VD, IVD);
      } if (VarUnreadUnmodified.count(VD)) {
        UDG.diagnoseUnreadUnmodified(VD);
      } else if (VarReadUnmodified.count(VD)) {
        UDG.diagnoseReadUnmodified(VD);
      } else if (VarUnreadModified.count(VD)) {
        UDG.diagnoseUnreadModified(VD);
      }
    }
  }
  
  /// The entry point to the transformation.
  void run() override {
    checkVarUsage(*getFunction());
  }
};

} // end anonymous namespace

SILTransform *swift::createDiagnoseVarUsage() {
  return new DiagnoseVarUsage();
}
