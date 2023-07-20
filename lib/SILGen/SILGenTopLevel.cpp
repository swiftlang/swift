#include "SILGenTopLevel.h"
#include "SILGenFunction.h"
#include "Scope.h"
#include "swift/AST/DiagnosticsSIL.h"

#define DEBUG_TYPE "silgen"

using namespace swift;
using namespace Lowering;

/// Emit a `makr_function_escape_instruction` into `SGF` if `AFD` captures an
/// uninitialized global variable
static void emitMarkFunctionEscape(SILGenFunction &SGF,
                                   AbstractFunctionDecl *AFD) {
  if (AFD->getDeclContext()->isLocalContext())
    return;
  auto CaptureInfo = AFD->getCaptureInfo();
  SGF.emitMarkFunctionEscapeForTopLevelCodeGlobals(AFD, std::move(CaptureInfo));
}

SILGenTopLevel::SILGenTopLevel(SILGenFunction &SGF) : SGF(SGF) {}

void SILGenTopLevel::visitNominalTypeDecl(NominalTypeDecl *NTD) {
  TypeVisitor(SGF).emit(NTD);
}

void SILGenTopLevel::visitExtensionDecl(ExtensionDecl *ED) {
  ExtensionVisitor(SGF).emit(ED);
}

void SILGenTopLevel::visitAbstractFunctionDecl(AbstractFunctionDecl *AFD) {
  emitMarkFunctionEscape(SGF, AFD);
}

void SILGenTopLevel::visitAbstractStorageDecl(AbstractStorageDecl *ASD) {
  ASD->visitEmittedAccessors(
      [this](AccessorDecl *Accessor) { visitAbstractFunctionDecl(Accessor); });
}

void SILGenTopLevel::visitTopLevelCodeDecl(TopLevelCodeDecl *TD) {

  SGF.emitProfilerIncrement(TD->getBody());

  DebugScope DS(SGF, CleanupLocation(TD));

  for (auto &ESD : TD->getBody()->getElements()) {
    if (!SGF.B.hasValidInsertionPoint()) {
      if (auto *S = ESD.dyn_cast<Stmt *>()) {
        if (S->isImplicit())
          continue;
      } else if (auto *E = ESD.dyn_cast<Expr *>()) {
        if (E->isImplicit())
          continue;
      }

      SGF.SGM.diagnose(ESD.getStartLoc(), diag::unreachable_code);
      // There's no point in trying to emit anything else.
      return;
    }

    if (auto *S = ESD.dyn_cast<Stmt *>()) {
      SGF.emitStmt(S);
    } else if (auto *E = ESD.dyn_cast<Expr *>()) {
      SGF.emitIgnoredExpr(E);
    } else {
      SGF.visit(ESD.get<Decl *>());
    }
  }
}

SILGenTopLevel::TypeVisitor::TypeVisitor(SILGenFunction &SGF) : SGF(SGF) {}

void SILGenTopLevel::TypeVisitor::emit(IterableDeclContext *Ctx) {
  forEachMemberToLower(Ctx, [this](Decl *Member) { visit(Member); });
}

void SILGenTopLevel::TypeVisitor::visitPatternBindingDecl(
    PatternBindingDecl *PD) {
  for (auto i : range(PD->getNumPatternEntries())) {
    if (!PD->getExecutableInit(i) || PD->isStatic())
      continue;
    auto *Var = PD->getAnchoringVarDecl(i);
    if (Var->getDeclContext()->isLocalContext())
      continue;
    auto CaptureInfo = PD->getCaptureInfo(i);

    // If this is a stored property initializer inside a type at global scope,
    // it may close over a global variable. If we're emitting top-level code,
    // then emit a "mark_function_escape" that lists the captured global
    // variables so that definite initialization can reason about this
    // escape point.
    SGF.emitMarkFunctionEscapeForTopLevelCodeGlobals(Var,
                                                     std::move(CaptureInfo));
  }
}

void SILGenTopLevel::TypeVisitor::visitNominalTypeDecl(NominalTypeDecl *NTD) {
  TypeVisitor(SGF).emit(NTD);
}

void SILGenTopLevel::TypeVisitor::visitAbstractFunctionDecl(
    AbstractFunctionDecl *AFD) {
  emitMarkFunctionEscape(SGF, AFD);
}

void SILGenTopLevel::TypeVisitor::visitAbstractStorageDecl(
    AbstractStorageDecl *ASD) {
  ASD->visitEmittedAccessors(
      [this](AccessorDecl *Accessor) { visitAbstractFunctionDecl(Accessor); });
}

SILGenTopLevel::ExtensionVisitor::ExtensionVisitor(SILGenFunction &SGF)
    : TypeVisitor(SGF) {}

void SILGenTopLevel::ExtensionVisitor::visitPatternBindingDecl(
    PatternBindingDecl *PD) {
  auto *Ctx = PD->getDeclContext();
  if (isa<ExtensionDecl>(Ctx) &&
      cast<ExtensionDecl>(Ctx)->isObjCImplementation()) {
    TypeVisitor::visitPatternBindingDecl(PD);
  }
}
