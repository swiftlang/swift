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

#define DEBUG_TYPE "diagnose-var-usage"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                           MARK: Helper Functions
//===----------------------------------------------------------------------===//

static VarDecl *getVarDecl(SILInstruction *I) {
  if (auto Loc = I->getLoc()) {
    if (auto *VD = Loc.getAsASTNode<VarDecl>()) {
      return VD;
    }
  }
  return nullptr;
}

static bool isSetterParam(SILInstruction *I) {
  if (auto *VD = getVarDecl(I)) {
    if (auto PD = dyn_cast<ParamDecl>(VD)) {
      if (auto AD = dyn_cast<AccessorDecl>(VD->getDeclContext())) {
        if (AD->getAccessorKind() == AccessorKind::Set) {
          return true;
        }
      }
    }
  }
  return false;
}

// Returns the SILInstruction if it is part of a non-parameter VarDecl.
static bool verifyVarDecl(SILInstruction *I) {
  if (auto *VD = getVarDecl(I)) {
    
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
//      sawError = true;
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

static bool isSelfParam(SILInstruction *I) {
  if (auto *VD = getVarDecl(I)) {
    return VD->isSelfParameter();
  }
  return false;
}

static SILValue getDebugVarOp(SILInstruction *I) {
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

static SILValue getDebugVarAddrOp(SILInstruction *I) {
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

static SILInstruction *getDebugVar(SILInstruction *I) {
  if (auto *DVI = dyn_cast<DebugValueInst>(I)) {
    
    // If is a param, if AccessorDecl
    if (isSetterParam(I)) {
      return I;
    }
    
    if (DVI->getVarInfo().hasValue()) {
      if (DVI->getVarInfo().getValue().ArgNo) {
        
        // This debug var represents an argument, not the initial declaration
        return nullptr;
      }
    }
    return verifyVarDecl(DVI) ? DVI : nullptr; // FIXME: may be redundant
  }
  return nullptr;
}

static SILInstruction *getDeclaration(SILInstruction *I) {
  
  // FIXME: VarDecl are lowered to AllocBoxInst or EnumInst
  if (!isa<AllocBoxInst>(I) &&
      !isa<AllocStackInst>(I) &&
      !isa<DebugValueInst>(I) &&
      !isa<EnumInst>(I)) {
    return nullptr;
  }
  
  if (isa<DebugValueInst>(I)) {
    if (auto *DBV = getDebugVar(I)) {
      if (isSelfParam(I)) {
        return nullptr;
      }
      return verifyVarDecl(I) ? DBV : nullptr;
    }
    return nullptr;
  }
  
  return verifyVarDecl(I) ? I : nullptr;
}

static bool argIsInout(SILArgumentConvention ArgConv) {
  switch (ArgConv) {
    case SILArgumentConvention::Indirect_Inout:
    case SILArgumentConvention::Indirect_InoutAliasable:
      return true;
    default:
      return false;
  }
}

static SILValue getFunctionArgInst(SILInstruction *Apply,
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

static SILArgument *getBasicBlockArgInst(BranchInst *BI,
                                         SILValue Param) {
  
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
  
  void appendValues(SILInstruction *I) {
    for (SILValue Value : I->getResults()) {
      appendValue(Value);
    }
  }
  
  void appendValue(SILValue Value) {
    if (!Seen.count(Value)) {
      Worklist.push_back(Value);
      Seen.insert(Value);
    }
  }
  
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
    
    if (AS && Fn) {
      if ((Op->getUser() == Apply) && AS.isArgumentOperand(*Op)) {
        return argIsInout(AS.getArgumentConvention(*Op));
      }
    }
    return false;
  }
  
  bool isDirectModify(SILInstruction *I, Operand *Op) {
    switch (I->getKind()) {
        
      case SILInstructionKind::BeginAccessInst:
        return dyn_cast<BeginAccessInst>(I)->getAccessKind() ==
        SILAccessKind::Modify;
        
//      case SILInstructionKind::BeginUnpairedAccessInst:
//        return (dyn_cast<BeginUnpairedAccessInst>(I)->getAccessKind() == SILAccessKind::Modify);
        
      case SILInstructionKind::AddressToPointerInst:
//      case SILInstructionKind::PointerToAddressInst:
//      case SILInstructionKind::RefToRawPointerInst:
      // A weak var does not have to be modified, so for the intent of this
      // traversal, return true
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
  
  void search(SILInstruction *I, Operand *Op) {
    switch (I->getKind()) {
//      case SILInstructionKind::DestructureTupleInst: // No
//      case SILInstructionKind::InitExistentialRefInst: // No
//      case SILInstructionKind::LoadInst: // No
//      case SILInstructionKind::OpenExistentialValueInst: // No
//      case SILInstructionKind::OpenExistentialRefInst: // No
//      case SILInstructionKind::OpenExistentialMetatypeInst: // No
//      case SILInstructionKind::BeginUnpairedAccessInst:
//      case SILInstructionKind::StructExtractInst: // No
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
        if (auto Arg = getFunctionArgInst(I, Op)) {
          appendValue(Arg);
        }
        break;
      default:
        break;
    }
  }
  
public:
  
  bool didModify() {
    
    if (auto DVO = getDebugVarOp(EntryInst)) {
      appendValue(DVO);
    } else if (auto DVAO = getDebugVarAddrOp(EntryInst)) {
      appendValue(DVAO);
    }
    
    while (!Worklist.empty()) {
      
      auto Value = Worklist.pop_back_val();
      
      // Return Trivial Cases
      for (Operand *Use : Value->getUses()) {
        if (SILInstruction *User = Use->getUser()) {
          if (isDirectModify(User, Use)) {
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

class VariableReadTraverser {
  
  VarDecl *VD;
  SmallVector<SILValue, 32> Worklist;
  llvm::SetVector<SILValue> Seen;
  SILInstruction *EntryInst;
  
public:
  VariableReadTraverser(SILInstruction *EntryInst, VarDecl *VD) : VD(VD),
  EntryInst(EntryInst) {
    appendValues(EntryInst);
  }
  
private:
  
  void appendValues(SILInstruction *I) {
    for (SILValue Value : I->getResults()) {
      appendValue(Value);
    }
  }
  
  void appendValue(SILValue Value) {
    if (!Seen.count(Value)) {
      Worklist.push_back(Value);
      Seen.insert(Value);
    }
  }
  
  static bool hasClosureWithArguments(ApplySite AS) {
    for (auto &Op : AS.getArgumentOperands()) {
      auto Value = Op.get();
      if (auto FnType = Value->getType().getAs<SILFunctionType>()) {
        return FnType->getNumParameters();
      }
    }
    return false;
  }
  
  static bool externalFunctionArgIsRead(SILInstruction *Apply, Operand *Op) {
    
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
    
    // This function is declared externally, and we can
    // inspect argument conventions.
    if (externalFunctionArgIsRead(I, Op)) {
      return true;
    }
    
    return false;
  }
  
  bool isDirectRead(SILInstruction *I, Operand *Op, SILValue Value) {
    if (isSelfParam(I)) {
      return true;
    }
    
    switch (I->getKind()) {
      case SILInstructionKind::ReturnInst:
      case SILInstructionKind::BuiltinInst:  // FIXME: Is this true?
      case SILInstructionKind::CondBranchInst:
      case SILInstructionKind::ThrowInst:
      case SILInstructionKind::TupleInst:
      case SILInstructionKind::SwitchEnumInst:
      case SILInstructionKind::SwitchEnumAddrInst:
      case SILInstructionKind::UncheckedEnumDataInst: // FIXME: Should we search?
      case SILInstructionKind::StoreBorrowInst: // FIXME: Should we search?
      case SILInstructionKind::StoreWeakInst: // FIXME: Should we search?
      case SILInstructionKind::OpenExistentialAddrInst: // FIXME: Should we search?
      case SILInstructionKind::PointerToAddressInst:
      case SILInstructionKind::MarkDependenceInst:
        return true;
        
      case SILInstructionKind::StoreInst:
        return dyn_cast<StoreInst>(I)->getSrc() == Value;
      case SILInstructionKind::AssignInst:
        return dyn_cast<AssignInst>(I)->getSrc() == Value;
      case SILInstructionKind::CopyAddrInst:
        return dyn_cast<CopyAddrInst>(I)->getSrc() == Value;
        
      case SILInstructionKind::DebugValueInst:
        if (auto *DebugVD = getVarDecl(I)) {
          // This is an assignment to another variable
          if (DebugVD != VD) {
            return true;
          }
        }
        return false;
        
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
  
  void search(SILInstruction *I, Operand *Op, SILValue Value) {
    switch (I->getKind()) {
      case SILInstructionKind::BranchInst:
        if (auto *Arg = getBasicBlockArgInst(dyn_cast<BranchInst>(I), Value)) {
          appendValue(Arg);
        }
        break;
        
      case SILInstructionKind::ApplyInst:
      case SILInstructionKind::BeginApplyInst:
      case SILInstructionKind::PartialApplyInst:
      case SILInstructionKind::TryApplyInst:
        
        // FIXME: crashes if we are coming from an argument
        appendValues(I);
        
        // This function is defined locally, and we can step in to
        // track variable usage
        if (auto Arg = getFunctionArgInst(I, Op)) {
          appendValue(Arg);
        }
        break;
        
      default:
        appendValues(I);
    }
  }
  
public:
  
  bool didRead() {
    
    if (isSelfParam(EntryInst)) {
      return true;
    } else if (auto DVO = getDebugVarOp(EntryInst)) {
      appendValue(DVO);
    } else if (auto DVAO = getDebugVarAddrOp(EntryInst)) {
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
          if (isDirectRead(User, Use, Value)) {
            return true;
          }
        }
      }
      
      // Traverse def-use
      for (Operand *Use : Value->getUses()) {
        if (SILInstruction *User = Use->getUser()) {
          search(User, Use, Value);
        }
      }
    }
    return false;
  }
  
};

class SetterReadsGetterTraverser {
  
  SILFunction *SetterFn;
  SILInstruction *GetterInst;
  
public:
  SetterReadsGetterTraverser(SILFunction *SetterFn) : SetterFn(SetterFn) { }
  
  SILInstruction *getGetterInst() {
    return GetterInst;
  }
  
  bool findGetterAccess() {
    for (auto &bb : *SetterFn) {
      auto i = bb.begin(), e = bb.end();
      while (i != e) {
        SILInstruction *I = &*i;
        
        if (auto FRI = dyn_cast<FunctionRefInst>(I)) {
          if (auto Fn = FRI->getReferencedFunctionOrNull()) {
            if (auto AD = dyn_cast<AccessorDecl>(Fn->getDeclContext())) {
              if (AD->getAccessorKind() == AccessorKind::Get) {
                GetterInst = FRI;
                return true;
              }
            }
          }
        } else if (auto CMI = dyn_cast<ClassMethodInst>(I)) {
          if (auto AD = dyn_cast<AccessorDecl>(CMI->getMember().getDecl())) {
            if (AD->getAccessorKind() == AccessorKind::Get) {
              GetterInst = CMI;
              return true;
            }
          }
        }
        
        ++i;
      }
    }
    return false;
  }
  
  void diagnoseGetterUse(VarDecl *var) {
    
    auto &Context = SetterFn->getModule().getASTContext();
    
  Context.Diags.diagnose(GetterInst->getLoc().getSourceLoc(),
                           diag::unused_setter_parameter,
                           var->getName());
    Context.Diags.diagnose(GetterInst->getLoc().getSourceLoc(),
                           diag::fixit_for_unused_setter_parameter,
                           var->getName())
                 .fixItReplace(GetterInst->getLoc().getSourceRange(),
                               var->getName().str());
    
//    if (auto param = dyn_cast<ParamDecl>(var)) {
//      auto FD = dyn_cast<AccessorDecl>(param->getDeclContext());
//      if (FD && FD->getAccessorKind() == AccessorKind::Set) {
//        Context.Diags.diagnose(var->getLoc(), diag::unused_setter_parameter,
//                               var->getName());
//        Context.Diags.diagnose(var->getLoc(), diag::fixit_for_unused_setter_parameter,
//                               var->getName())
//        .fixItReplace(var->getSourceRange(), var->getName().str());
//      }
//      return;
//    }
  }
};

static bool didModify(VarDecl *VD, SILInstruction *I) {
  auto VMT = VariableModifyTraverser(I);
  return VMT.didModify();
}

static bool didRead(VarDecl *VD, SILInstruction *I) {
  
  auto VRT = VariableReadTraverser(I, VD);
  return VRT.didRead();
}

static void searchSetterParam(VarDecl *VD, SILInstruction *I) {
  
  auto VRT = VariableReadTraverser(I, VD);
  if (!VRT.didRead()) {
    auto SRGT = SetterReadsGetterTraverser(I->getFunction());
    if (SRGT.findGetterAccess()) {
      SRGT.diagnoseGetterUse(VD);
    }
  }
}

//===----------------------------------------------------------------------===//
//                         MARK: Diagostics Generator
//===----------------------------------------------------------------------===//

class DiagnoseVarUsageCollector {
  ASTContext &Context;
  
public:
  
  DiagnoseVarUsageCollector(ASTContext &Context) : Context(Context) {}
  
  void diagnoseReadModified(VarDecl *var) {
    // If this is a member in a capture list, just say it is unused.  We could
    // produce a fixit hint with a parent map, but this is a lot of effort for
    // a narrow case.
    //    if (var->isCaptureList()) {
    //      Context.Diags.diagnose(var->getLoc(), diag::capture_never_used,
    //                             var->getName());
    //    }
  }
  
  void diagnoseUnreadUnmodified(VarDecl *var, bool Modified = false) {
    
//    if (auto param = dyn_cast<ParamDecl>(var)) {
//      auto FD = dyn_cast<AccessorDecl>(param->getDeclContext());
//      if (FD && FD->getAccessorKind() == AccessorKind::Set) {
//        Context.Diags.diagnose(var->getLoc(), diag::unused_setter_parameter,
//                               var->getName());
//        Context.Diags.diagnose(var->getLoc(), diag::fixit_for_unused_setter_parameter,
//                               var->getName())
//        .fixItReplace(var->getSourceRange(), var->getName().str());
//      }
//      return;
//    }
    
    // If the source of the VarDecl is a trivial PatternBinding with only a
    // single binding, rewrite the whole thing into an assignment.
    //    let x = foo()
    //  ->
    //    _ = foo()
    if (auto *pbd = var->getParentPatternBinding()) {
      if (pbd->getSingleVar() == var && pbd->getInit(0) != nullptr &&
          !isa<TypedPattern>(pbd->getPattern(0))) {
        unsigned varKind = var->isLet();
        SourceRange replaceRange(
                                 pbd->getStartLoc(),
                                 pbd->getPattern(0)->getEndLoc());
        Context.Diags.diagnose(var->getLoc(), diag::pbd_never_used,
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
                  
                  auto diagIF = Context.Diags.diagnose(var->getLoc(),
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
        Context.Diags.diagnose(var->getLoc(), diag::variable_never_used,
                               var->getName(), varKind)
        .fixItReplace(foundVP->getSourceRange(), "_");
        return;
      }
    }
    
    // Otherwise, this is something more complex, perhaps
    //    let (a,b) = foo()
    if (var->isLet() && Modified) {
      Context.Diags.diagnose(var->getLoc(),
                             diag::immutable_value_never_used_but_assigned,
                             var->getName());
    } else {
      unsigned varKind = var->isLet();
      // Just rewrite the one variable with a _.
      Context.Diags.diagnose(var->getLoc(), diag::variable_never_used,
                             var->getName(), varKind)
      .fixItReplace(var->getLoc(), "_");
    }
  }
  
  void diagnoseUnreadModified(VarDecl *var) {
    
    //    diagnoseUnreadUnmodified(var, true);
    // If this is a variable that was only written to, emit a warning.
    Context.Diags.diagnose(var->getLoc(),
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
      Context.Diags.diagnose(var->getLoc(), diag::variable_never_mutated,
                             var->getName(), true);
    } else {
      bool suggestLet = true;
      if (auto *stmt = var->getRecursiveParentPatternStmt()) {
        // Don't try to suggest 'var' -> 'let' conversion
        // in case of 'for' loop because it's an implicitly
        // immutable context.
        suggestLet = !isa<ForEachStmt>(stmt);
      }
      
      auto diag = Context.Diags.diagnose(var->getLoc(), diag::variable_never_mutated,
                                         var->getName(), suggestLet);
      
      if (suggestLet)
        diag.fixItReplace(FixItLoc, "let");
      else
        diag.fixItRemove(FixItLoc);
    }
  }
  
};


//===----------------------------------------------------------------------===//
//                           MARK: Top Level Driver
//===----------------------------------------------------------------------===//

/// Check that all memory objects that require initialization before use are
/// properly set and transform the code as required for flow-sensitive
/// properties.
static void checkVarUsage(SILFunction &Fn) {
  LLVM_DEBUG(llvm::dbgs() << "*** Diagnose Var Usage visiting function: "
             <<  Fn.getName() << "\n");
  
  SILModule &M = Fn.getModule();
  
  DiagnoseVarUsageCollector DVUC(M.getASTContext());
  
  llvm::SetVector<VarDecl *> VarAll;
  llvm::SetVector<VarDecl *> VarUnreadUnmodified;
  llvm::SetVector<VarDecl *> VarReadUnmodified;
  llvm::SetVector<VarDecl *> VarUnreadModified;
  llvm::SetVector<VarDecl *> VarReadModified;
  
  for (auto &bb : Fn) {
    auto i = bb.begin(), e = bb.end();
    while (i != e) {
      
      SILInstruction *Inst = getDeclaration(&*i);
      if (!Inst) {
        ++i;
        continue;
      }
      
      VarDecl *VD = getVarDecl(Inst);
      
      if (isSetterParam(Inst)) {
        searchSetterParam(VD, Inst);
        ++i;
        continue;
      }
      
      if (!VarAll.count(VD)) {
        VarAll.insert(VD);
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
        if (didModify(VD, Inst)) {
          VarReadUnmodified.remove(VD);
          VarReadModified.insert(VD);
        }
        ++i;
        continue;
      }
      
      // Pop unread and modified declarations to consider promotion to read
      // and modified.
      if (VarUnreadModified.count(VD)) {
        if (didRead(VD, Inst)) {
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
      bool DidRead = didRead(VD, Inst);
      bool DidModify = didModify(VD, Inst);
      if (!DidRead && !DidModify) {
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
    
    // TODO: explain better
    // Promote modifications, but do not promote reads
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
  
  for (VarDecl *VD : VarAll) {
    if (VarUnreadUnmodified.count(VD)) {
      DVUC.diagnoseUnreadUnmodified(VD, false);
    } else if (VarReadUnmodified.count(VD)) {
      DVUC.diagnoseReadUnmodified(VD);
    } else if (VarUnreadModified.count(VD)) {
      DVUC.diagnoseUnreadModified(VD);
    } else if (VarReadModified.count(VD)) {
      DVUC.diagnoseReadModified(VD);
    }
  }
}

namespace {

/// Perform definitive initialization analysis and promote alloc_box uses into
/// SSA registers for later SSA-based dataflow passes.
class DiagnoseVarUsage : public SILFunctionTransform {
  
  /// The entry point to the transformation.
  void run() override {
    //    // Don't rerun diagnostics on deserialized functions.
    //    if (getFunction()->wasDeserializedCanonical())
    //      return;
    
    checkVarUsage(*getFunction());
  }
};

} // end anonymous namespace

SILTransform *swift::createDiagnoseVarUsage() {
  return new DiagnoseVarUsage();
}
