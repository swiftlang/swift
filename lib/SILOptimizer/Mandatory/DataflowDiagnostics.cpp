//===--- DataflowDiagnostics.cpp - Emits diagnostics based on SIL analysis ===//
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

#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/Syntax/TokenKinds.h"

using namespace swift;

template<typename...T, typename...U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
              U &&...args) {
  Context.Diags.diagnose(loc,
                         diag, std::forward<U>(args)...);
}

static void diagnoseMissingReturn(const UnreachableInst *UI,
                                  ASTContext &Context) {
  const SILBasicBlock *BB = UI->getParent();
  const SILFunction *F = BB->getParent();
  SILLocation FLoc = F->getLocation();

  Type ResTy;

  if (auto *FD = FLoc.getAsASTNode<FuncDecl>()) {
    ResTy = FD->getResultInterfaceType();
  } else if (auto *CE = FLoc.getAsASTNode<ClosureExpr>()) {
    ResTy = CE->getResultType();
  } else {
    llvm_unreachable("unhandled case in MissingReturn");
  }

  SILLocation L = UI->getLoc();
  assert(L && ResTy);
  auto diagID = F->isNoReturnFunction() ? diag::missing_never_call
                                        : diag::missing_return;
  diagnose(Context,
           L.getEndSourceLoc(),
           diagID, ResTy,
           FLoc.isASTNode<ClosureExpr>() ? 1 : 0);
}


/// If the pattern handles an enum element, remove the enum element from the
/// given set. If seeing uncertain patterns, e.g. any pattern, return true;
/// otherwise return false.
static bool
removedHandledEnumElements(Pattern *P,
                           llvm::DenseSet<EnumElementDecl*> &UnhandledElements) {
  if (auto *EEP = dyn_cast<EnumElementPattern>(P)) {
    UnhandledElements.erase(EEP->getElementDecl());
  } else if (auto *VP = dyn_cast<VarPattern>(P)) {
    return removedHandledEnumElements(VP->getSubPattern(), UnhandledElements);
  } else {
    return true;
  }
  return false;
};

static void diagnoseMissingCases(ASTContext &Context,
                                 const SwitchStmt *SwitchS) {
  SourceLoc EndLoc = SwitchS->getEndLoc();
  StringRef Placeholder = "<#Code#>";
  SmallString<32> Buffer;
  llvm::raw_svector_ostream OS(Buffer);

  auto DefaultDiag = [&]() {
    OS << tok::kw_default << ": " << Placeholder << "\n";
    Context.Diags.diagnose(EndLoc, diag::non_exhaustive_switch).
      fixItInsert(EndLoc, Buffer.str());
  };
  // To find the subject enum decl for this switch statement.
  EnumDecl *SubjectED = nullptr;
  if (auto SubjectTy = SwitchS->getSubjectExpr()->getType()) {
    if (auto *ND = SubjectTy->getAnyNominal()) {
      SubjectED = ND->getAsEnumOrEnumExtensionContext();
    }
  }

  // The switch is not about an enum decl or about an Optional decl,
  // add "default:" instead.
  if (!SubjectED) {
    DefaultDiag();
    return;
  }

  // Assume enum elements are not handled in the switch statement.
  llvm::DenseSet<EnumElementDecl*> UnhandledElements;

  SubjectED->getAllElements(UnhandledElements);
  for (auto Current : SwitchS->getCases()) {
    // For each handled enum element, remove it from the bucket.
    for (auto Item : Current->getCaseLabelItems()) {
      if (removedHandledEnumElements(Item.getPattern(), UnhandledElements)) {
        DefaultDiag();
        return;
      }
    }
  }

  // No unhandled cases, so we are not sure the exact case to add, fall back to
  // default instead.
  if (UnhandledElements.empty()) {
    DefaultDiag();
    return;
  }

  // Sort the missing elements to a vector because set does not guarantee orders.
  SmallVector<EnumElementDecl*, 4> SortedElements;
  SortedElements.insert(SortedElements.begin(), UnhandledElements.begin(),
                        UnhandledElements.end());
  std::sort(SortedElements.begin(),SortedElements.end(),
            [](EnumElementDecl *LHS, EnumElementDecl *RHS) {
              return LHS->getNameStr().compare(RHS->getNameStr()) < 0;
            });

  // Print each enum element name.
  std::for_each(SortedElements.begin(), SortedElements.end(),
                [&](EnumElementDecl *EE) {
    OS << tok::kw_case << " ." << EE->getNameStr() << ": " <<
      Placeholder << "\n";
  });
  Context.Diags.diagnose(EndLoc, diag::non_exhaustive_switch).
    fixItInsert(EndLoc, Buffer.str());
}

static void diagnoseUnreachable(const SILInstruction *I,
                                ASTContext &Context) {
  if (auto *UI = dyn_cast<UnreachableInst>(I)) {
    SILLocation L = UI->getLoc();

    // Invalid location means that the instruction has been generated by SIL
    // passes, such as DCE. FIXME: we might want to just introduce a separate
    // instruction kind, instead of keeping this invariant.
    //
    // We also do not want to emit diagnostics for code that was
    // transparently inlined. We should have already emitted these
    // diagnostics when we process the callee function prior to
    // inlining it.
    if (!L || L.is<MandatoryInlinedLocation>())
      return;

    // The most common case of getting an unreachable instruction is a
    // missing return statement. In this case, we know that the instruction
    // location will be the enclosing function.
    if (L.isASTNode<AbstractFunctionDecl>() || L.isASTNode<ClosureExpr>()) {
      diagnoseMissingReturn(UI, Context);
      return;
    }

    // A non-exhaustive switch would also produce an unreachable instruction.
    if (L.isASTNode<SwitchStmt>()) {
      diagnoseMissingCases(Context, L.getAsASTNode<SwitchStmt>());
      return;
    }

    if (auto *Guard = L.getAsASTNode<GuardStmt>()) {
      diagnose(Context, Guard->getBody()->getEndLoc(),
               diag::guard_body_must_not_fallthrough);
      return;
    }
  }
}

/// \brief Issue diagnostics whenever we see Builtin.static_report(1, ...).
static void diagnoseStaticReports(const SILInstruction *I,
                                  SILModule &M) {

  // Find out if we are dealing with Builtin.staticReport().
  if (auto *BI = dyn_cast<BuiltinInst>(I)) {
    const BuiltinInfo &B = BI->getBuiltinInfo();
    if (B.ID == BuiltinValueKind::StaticReport) {

      // Report diagnostic if the first argument has been folded to '1'.
      OperandValueArrayRef Args = BI->getArguments();
      IntegerLiteralInst *V = dyn_cast<IntegerLiteralInst>(Args[0]);
      if (!V || V->getValue() != 1)
        return;

      diagnose(M.getASTContext(), I->getLoc().getSourceLoc(),
               diag::static_report_error);
    }
  }
}

namespace {
class EmitDFDiagnostics : public SILFunctionTransform {
  ~EmitDFDiagnostics() override {}

  StringRef getName() override { return "Emit Dataflow Diagnostics"; }

  /// The entry point to the transformation.
  void run() override {
    SILModule &M = getFunction()->getModule();
    for (auto &BB : *getFunction())
      for (auto &I : BB) {
        diagnoseUnreachable(&I, M.getASTContext());
        diagnoseStaticReports(&I, M);
      }
  }
};
} // end anonymous namespace


SILTransform *swift::createEmitDFDiagnostics() {
  return new EmitDFDiagnostics();
}
