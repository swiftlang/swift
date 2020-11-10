//===--- DiagnoseVarUsage.cpp - Perform definite init analysis ------===//
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

#define DEBUG_TYPE "definite-init"
#include "DVUMemoryUseCollector.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/ClangImporter/ClangModule.h"
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
// Per-Element Promotion Logic
//===----------------------------------------------------------------------===//

class DiagnoseVarUsageCollector {
  ASTContext &Context;
  llvm::SetVector<VarDecl *> Diagnosed;
  
public:
  
  DiagnoseVarUsageCollector(ASTContext &Context) : Context(Context) {}
  
  void diagnoseOther(const SILInstruction *I) {
    SILLocation Loc = I->getLoc();
    if (auto *var = Loc.getAsASTNode<VarDecl>()) {
      if (Diagnosed.count(var)) {
        return;
      }
      
      // If this is a member in a capture list, just say it is unused.  We could
      // produce a fixit hint with a parent map, but this is a lot of effort for
      // a narrow case.
      if (var->isCaptureList()) {
        Context.Diags.diagnose(var->getLoc(), diag::capture_never_used,
                               var->getName());
        Diagnosed.insert(var);
      }
    }
  }
  
  void diagnoseUnused(const SILInstruction *I, unsigned Stores) {
    SILLocation Loc = I->getLoc();
    if (auto *var = Loc.getAsASTNode<VarDecl>()) {
      
      if (Diagnosed.count(var)) {
        return;
      }
      Diagnosed.insert(var);
      
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
      if (var->isLet() && Stores > 1) {
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
  }
  
  void handleNeverMutated(const SILInstruction *I) {
    SILLocation Loc = I->getLoc();
    if (auto *var = Loc.getAsASTNode<VarDecl>()) {
      
      if (Diagnosed.count(var)) {
        return;
      }
      Diagnosed.insert(var);
      
      if (!var->isLet()) {
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
    }
  }
};

static bool isValidPropertyLoad(SILInstruction *I) {
  if (isIncidentalUse(I))
    return true;
  if (isa<LoadInst>(I))
    return true;
  if (isa<CopyAddrInst>(I))
    return true;
  if (isa<ApplyInst>(I))
    return true;
  if (isa<OpenExistentialAddrInst>(I))
    return true;
  
  if (isa<StructElementAddrInst>(I) || isa<TupleElementAddrInst>(I)
      || isa<BeginAccessInst>(I)) {
//    auto projection = cast<SingleValueInstruction>(I);
//    for (auto Use : getNonDebugUses(projection)) {
//      if (!isValidPropertyLoad(Use->getUser()))
//        return false;
//    }
    return true;
  }
  
  return false;
}

//===----------------------------------------------------------------------===//
//                           Top Level Driver
//===----------------------------------------------------------------------===//
static SILInstruction *getVar(SILInstruction *i) {
  
  if (
      !isa<AllocBoxInst>(i) &&
      !isa<AllocStackInst>(i) &&
      !isa<StructInst>(i) &&
      !isa<TupleInst>(i)) {
    return nullptr;
  }
  
  SILLocation Loc = i->getLoc();
  if (auto *var = Loc.getAsASTNode<VarDecl>()) {
    return i;
  }
  
  return nullptr;
}

/// Check that all memory objects that require initialization before use are
/// properly set and transform the code as required for flow-sensitive
/// properties.
static void checkVarUsage(SILFunction &Fn) {
  LLVM_DEBUG(llvm::dbgs() << "*** Diagnose Var Usage visiting function: "
             <<  Fn.getName() << "\n");
  
  SILModule &M = Fn.getModule();
  
  DiagnoseVarUsageCollector DVUC(M.getASTContext());
  
  for (auto &bb : Fn) {
    auto i = bb.begin(), e = bb.end();
    while (i != e) {
      
      SILInstruction *Inst = getVar(&*i);
      if (!Inst) {
        ++i;
        continue;
      }
      
      unsigned Stores = 1;
      bool DidLoad = false;
      
      if (!Inst->hasResults())
        assert(false);
      
      for (SILValue Result : Inst->getResults()) {
        for (Operand *I : getNonDebugUses(Result)) {
          auto *User = I->getUser();
          
          if (isValidPropertyLoad(User))
            DidLoad = true;
          
          if (dyn_cast<StoreInst>(User)) 
            DidLoad = true;
//            ++Stores;
          
          if (DidLoad && Stores > 1)
            break;
        }
      }
      
      if (!DidLoad) {
        DVUC.diagnoseUnused(Inst, Stores);
      } else if (Stores < 2) {
//        DVUC.handleNeverMutated(Inst);
      } else {
//        DVUC.diagnoseOther(Inst);
      }
      ++i;
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
    
    // Walk through and promote all of the alloc_box's that we can.
    checkVarUsage(*getFunction());
  }
};

} // end anonymous namespace

SILTransform *swift::createDiagnoseVarUsage() {
  return new DiagnoseVarUsage();
}
