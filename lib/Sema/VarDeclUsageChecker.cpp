//===--- VarDeclUsageChecker.cpp ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "MiscDiagnostics.h"
#include "TypeCheckAvailability.h"
#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

namespace {

class VarDeclUsageChecker : public ASTWalker {
  DiagnosticEngine &diags;

  // Keep track of some information about a variable.
  enum {
    RK_Read = 1,    ///< Whether it was ever read.
    RK_Written = 2, ///< Whether it was ever written or passed inout.

    RK_CaptureList = 4 ///< Var is an entry in a capture list.
  };

  /// These are all of the variables that we are tracking.  VarDecls get added
  /// to this when the declaration is seen.  We use a MapVector to keep the
  /// diagnostics emission in deterministic order.
  llvm::SmallMapVector<VarDecl *, unsigned, 32> varDecls;

  /// This is a mapping from an OpaqueValue to the expression that initialized
  /// it.
  llvm::SmallDenseMap<OpaqueValueExpr *, Expr *> opaqueValueMap;

  /// The getter associated with a setter function declaration.
  const VarDecl *associatedGetter = nullptr;

  /// The first reference to the associated getter.
  const Expr *associatedGetterRefExpr = nullptr;

  /// This is a mapping from VarDecls to the if/while/guard statement that they
  /// occur in, when they are in a pattern in a StmtCondition.
  llvm::SmallDenseMap<VarDecl *, LabeledConditionalStmt *> stmtConditionForVD;

  bool sawError = false;

  VarDeclUsageChecker(const VarDeclUsageChecker &) = delete;
  void operator=(const VarDeclUsageChecker &) = delete;

public:
  VarDeclUsageChecker(TypeChecker &tc, AbstractFunctionDecl *afd)
      : diags(tc.Diags) {
    // If this AFD is a setter, track the parameter and the getter for
    // the containing property so if newValue isn't used but the getter is used
    // an error can be reported.
    if (auto *accessorDecl = dyn_cast<AccessorDecl>(afd)) {
      if (accessorDecl->getAccessorKind() == AccessorKind::Set) {
        if (auto getter = dyn_cast<VarDecl>(accessorDecl->getStorage())) {
          auto arguments = accessorDecl->getParameters();
          varDecls[arguments->get(0)] = 0;
          associatedGetter = getter;
        }
      }
    }
  }

  VarDeclUsageChecker(DiagnosticEngine &diags) : diags(diags) {}

  VarDeclUsageChecker(TypeChecker &tc, VarDecl *vd) : diags(tc.Diags) {
    // Track a specific VarDecl
    varDecls[vd] = 0;
  }

  void suppressDiagnostics() {
    sawError =
        true; // set this flag so that no diagnostics will be emitted on delete.
  }

  // After we have scanned the entire region, diagnose variables that could be
  // declared with a narrower usage kind.
  ~VarDeclUsageChecker() override;

  /// Check to see if the specified VarDecl is part of a larger
  /// PatternBindingDecl, where some other bound variable was mutated.  In this
  /// case we don't want to generate a "variable never mutated" warning, because
  /// it would require splitting up the destructuring of the tuple, which is
  ///  more code turmoil than the warning is worth.
  bool isVarDeclPartOfPBDThatHadSomeMutation(VarDecl *vd) {
    auto *pbd = vd->getParentPatternBinding();
    if (!pbd)
      return false;

    bool sawMutation = false;
    for (const auto &pbe : pbd->getPatternList()) {
      pbe.getPattern()->forEachVariable([&](VarDecl *vd) {
        auto it = varDecls.find(vd);
        sawMutation |= it != varDecls.end() && (it->second & RK_Written);
      });
    }
    return sawMutation;
  }

  bool isVarDeclEverWritten(VarDecl *vd) {
    return (varDecls[vd] & RK_Written) != 0;
  }

  bool shouldTrackVarDecl(VarDecl *vd) {
    // If the variable is implicit, ignore it.
    if (vd->isImplicit() || vd->getLoc().isInvalid())
      return false;

    // If the variable is computed, ignore it.
    if (!vd->hasStorage())
      return false;

    // If the variable was invalid, ignore it and notice that the code is
    // malformed.
    if (vd->isInvalid() || !vd->hasType()) {
      sawError = true;
      return false;
    }

    // If the variable is already unnamed, ignore it.
    if (!vd->hasName() || vd->getName().str() == "_")
      return false;

    return true;
  }

  void addMark(Decl *d, unsigned flag) {
    auto *vd = dyn_cast<VarDecl>(d);
    if (!vd)
      return;

    auto vdi = varDecls.find(vd);
    if (vdi != varDecls.end())
      vdi->second |= flag;
  }

  void markBaseOfAbstractStorageDeclStore(Expr *e, ConcreteDeclRef decl);

  void markStoredOrInOutExpr(Expr *e, unsigned flags);

  // We generally walk into declarations, other than types and nested functions.
  // FIXME: peek into capture lists of nested functions.
  bool walkToDeclPre(Decl *d) override;

  /// The heavy lifting happens when visiting expressions.
  std::pair<bool, Expr *> walkToExprPre(Expr *e) override;

  /// handle #if directives.
  void handleIfConfig(IfConfigDecl *icd);

  /// Custom handling for statements.
  std::pair<bool, Stmt *> walkToStmtPre(Stmt *s) override;
};

} // end anonymous namespace

std::pair<bool, Stmt *> VarDeclUsageChecker::walkToStmtPre(Stmt *s) {
  // Keep track of an association between vardecls and the StmtCondition that
  // they are bound in for IfStmt, GuardStmt, WhileStmt, etc.
  if (auto *lcs = dyn_cast<LabeledConditionalStmt>(s)) {
    for (auto &cond : lcs->getCond())
      if (auto *pat = cond.getPatternOrNull()) {
        pat->forEachVariable(
            [&](VarDecl *vd) { stmtConditionForVD[vd] = lcs; });
      }
  }

  // A fallthrough dest case's bound variable means the source case's
  // var of the same name is read.
  if (auto *fallthroughStmt = dyn_cast<FallthroughStmt>(s)) {
    if (auto *sourceCase = fallthroughStmt->getFallthroughSource()) {
      SmallVector<VarDecl *, 4> sourceVars;
      auto sourcePattern = sourceCase->getCaseLabelItems()[0].getPattern();
      sourcePattern->collectVariables(sourceVars);

      auto destCase = fallthroughStmt->getFallthroughDest();
      auto destPattern = destCase->getCaseLabelItems()[0].getPattern();
      destPattern->forEachVariable([&](VarDecl *V) {
        if (!V->hasName())
          return;
        for (auto *var : sourceVars) {
          if (var->hasName() && var->getName() == V->getName()) {
            varDecls[var] |= RK_Read;
            break;
          }
        }
      });
    }
  }

  return {true, s};
}

bool VarDeclUsageChecker::walkToDeclPre(Decl *d) {
  if (isa<TypeDecl>(d))
    return false;

  // The body of #if clauses are not walked into, we need custom processing
  // for them.
  if (auto *icd = dyn_cast<IfConfigDecl>(d))
    handleIfConfig(icd);

  // If this is a VarDecl, then add it to our list of things to track.
  if (auto *vd = dyn_cast<VarDecl>(d))
    if (shouldTrackVarDecl(vd)) {
      unsigned defaultFlags = 0;
      // If this VarDecl is nested inside of a CaptureListExpr, remember that
      // fact for better diagnostics.
      auto parentAsExpr = Parent.getAsExpr();
      if (parentAsExpr && isa<CaptureListExpr>(parentAsExpr))
        defaultFlags = RK_CaptureList;
      varDecls[vd] |= defaultFlags;
    }

  if (auto *afd = dyn_cast<AbstractFunctionDecl>(d)) {
    // If this is a nested function with a capture list, mark any captured
    // variables.
    if (afd->isBodyTypeChecked()) {
      for (const auto &capture : afd->getCaptureInfo().getCaptures())
        addMark(capture.getDecl(), RK_Read | RK_Written);
    } else {
      // If the body hasn't been type checked yet, be super-conservative and
      // mark all variables as used.  This can be improved later, e.g. by
      // walking the untype-checked body to look for things that could
      // possibly be used.
      varDecls.clear();
    }

    // Don't walk into it though, it may not even be type checked yet.
    return false;
  }

  if (auto *tlcd = dyn_cast<TopLevelCodeDecl>(d)) {
    // If this is a TopLevelCodeDecl, scan for global variables
    auto *body = tlcd->getBody();
    for (auto node : body->getElements()) {
      if (node.is<Decl *>()) {
        // Flag all variables in a PatternBindingDecl
        auto *pbd = dyn_cast<PatternBindingDecl>(node.get<Decl *>());
        if (!pbd)
          continue;
        for (PatternBindingEntry pbe : pbd->getPatternList()) {
          pbe.getPattern()->forEachVariable(
              [&](VarDecl *vd) { varDecls[vd] = RK_Read | RK_Written; });
        }
      } else if (node.is<Stmt *>()) {
        // Flag all variables in guard statements
        auto *gs = dyn_cast<GuardStmt>(node.get<Stmt *>());
        if (!gs)
          continue;
        for (StmtConditionElement sce : gs->getCond()) {
          if (auto *pattern = sce.getPatternOrNull()) {
            pattern->forEachVariable(
                [&](VarDecl *vd) { varDecls[vd] = RK_Read | RK_Written; });
          }
        }
      }
    }
  }

  // Note that we ignore the initialization behavior of PatternBindingDecls,
  // but we do want to walk into them, because we want to see any uses or
  // other things going on in the initializer expressions.
  return true;
}

// After we have scanned the entire region, diagnose variables that could be
// declared with a narrower usage kind.
VarDeclUsageChecker::~VarDeclUsageChecker() {
  // If we saw an ErrorExpr somewhere in the body, then we have a malformed AST
  // and we know stuff got dropped.  Instead of producing these diagnostics,
  // lets let the bigger issues get resolved first.
  if (sawError)
    return;

  for (auto elt : varDecls) {
    auto *var = elt.first;
    unsigned access = elt.second;

    // If this is a 'let' value, any stores to it are actually initializations,
    // not mutations.
    auto isWrittenLet = false;
    if (var->isImmutable()) {
      isWrittenLet = (access & RK_Written) != 0;
      access &= ~RK_Written;
    }

    // If this variable has WeakStorageType, then it can be mutated in ways we
    // don't know.
    if (var->getType()->is<WeakStorageType>())
      access |= RK_Written;

    // If this is a vardecl with 'inout' type, then it is an inout argument to a
    // function, never diagnose anything related to it.
    if (var->isInOut())
      continue;

    // If the setter parameter is not used, but the property is read, report
    // a warning. Otherwise, parameters should not generate usage warnings. It
    // is common to name a parameter and not use it (e.g. because you are an
    // override or want the named keyword, etc).  Warning to rewrite it to _ is
    // more annoying than it is useful.
    if (auto param = dyn_cast<ParamDecl>(var)) {
      auto *accessorDecl = dyn_cast<AccessorDecl>(param->getDeclContext());
      if (accessorDecl &&
          accessorDecl->getAccessorKind() == AccessorKind::Set) {
        auto *getter = dyn_cast<VarDecl>(accessorDecl->getStorage());
        if ((access & RK_Read) == 0 && associatedGetter == getter) {
          if (auto dre = associatedGetterRefExpr) {
            diags.diagnose(dre->getLoc(), diag::unused_setter_parameter,
                           var->getName());
            diags
                .diagnose(dre->getLoc(),
                          diag::fixit_for_unused_setter_parameter,
                          var->getName())
                .fixItReplace(dre->getSourceRange(), var->getName().str());
          }
        }
      }
      continue;
    }

    // Diagnose variables that were never used (other than their
    // initialization).
    //
    if ((access & (RK_Read | RK_Written)) == 0) {
      // If this is a member in a capture list, just say it is unused.  We could
      // produce a fixit hint with a parent map, but this is a lot of effort for
      // a narrow case.
      if (access & RK_CaptureList) {
        diags.diagnose(var->getLoc(), diag::capture_never_used, var->getName());
        continue;
      }

      // If the source of the VarDecl is a trivial PatternBinding with only a
      // single binding, rewrite the whole thing into an assignment.
      //    let x = foo()
      //  ->
      //    _ = foo()
      if (auto *pbd = var->getParentPatternBinding())
        if (pbd->getSingleVar() == var && pbd->getInit(0) != nullptr &&
            !isa<TypedPattern>(pbd->getPatternList()[0].getPattern())) {
          unsigned varKind = var->isLet();
          SourceRange replaceRange(
              pbd->getStartLoc(),
              pbd->getPatternList()[0].getPattern()->getEndLoc());
          diags
              .diagnose(var->getLoc(), diag::pbd_never_used, var->getName(),
                        varKind)
              .fixItReplace(replaceRange, "_");
          continue;
        }

      // If the variable is defined in a pattern in an if/while/guard statement,
      // see if we can produce a tuned fixit.  When we have something like:
      //
      //    if let x = <expr> {
      //
      // we prefer to rewrite it to:
      //
      //    if <expr> != nil {
      //
      if (auto *stmtCondition = stmtConditionForVD[var]) {
        // We only handle the "if let" case right now, since it is vastly the
        // most common situation that people run into.
        if (stmtCondition->getCond().size() == 1) {
          auto pattern = stmtCondition->getCond()[0].getPattern();
          if (auto *osp = dyn_cast<OptionalSomePattern>(pattern)) {
            if (auto *lp = dyn_cast<VarPattern>(osp->getSubPattern())) {
              if (isa<NamedPattern>(lp->getSubPattern())) {
                auto initExpr = stmtCondition->getCond()[0].getInitializer();
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
                  auto introducerLoc =
                      stmtCondition->getCond()[0].getIntroducerLoc();
                  diagIF.fixItReplaceChars(
                      introducerLoc, initExpr->getStartLoc(), &"("[noParens]);

                  if (isIsTest) {
                    // If this was an "x as? T" check, rewrite it to "x is T".
                    auto CCE = cast<ConditionalCheckedCastExpr>(initExpr);
                    diagIF.fixItReplace(
                        SourceRange(CCE->getLoc(), CCE->getQuestionLoc()),
                        "is");
                  } else {
                    diagIF.fixItInsertAfter(initExpr->getEndLoc(),
                                            &") != nil"[noParens]);
                  }
                  continue;
                }
              }
            }
          }
        }
      }

      // Otherwise, this is something more complex, perhaps
      //    let (a,b) = foo()
      if (isWrittenLet) {
        diags.diagnose(var->getLoc(),
                       diag::immutable_value_never_used_but_assigned,
                       var->getName());
      } else {
        unsigned varKind = var->isLet();
        // Just rewrite the one variable with a _.
        diags
            .diagnose(var->getLoc(), diag::variable_never_used, var->getName(),
                      varKind)
            .fixItReplace(var->getLoc(), "_");
      }
      continue;
    }

    // If this is a mutable 'var', and it was never written to, suggest
    // upgrading to 'let'.  We do this even for a parameter.
    if (!var->isImmutable() && (access & RK_Written) == 0 &&
        // Don't warn if we have something like "let (x,y) = ..." and 'y' was
        // never mutated, but 'x' was.
        !isVarDeclPartOfPBDThatHadSomeMutation(var)) {
      SourceLoc fixItLoc;

      // Try to find the location of the 'var' so we can produce a fixit.  If
      // this is a simple PatternBinding, use its location.
      if (auto *pbd = var->getParentPatternBinding()) {
        if (pbd->getSingleVar() == var)
          fixItLoc = pbd->getLoc();
      } else if (auto *pattern = var->getParentPattern()) {
        VarPattern *foundVP = nullptr;
        pattern->forEachNode([&](Pattern *p) {
          if (auto *vp = dyn_cast<VarPattern>(p))
            if (vp->getSingleVar() == var)
              foundVP = vp;
        });

        if (foundVP && !foundVP->isLet())
          fixItLoc = foundVP->getLoc();
      }

      // If this is a parameter explicitly marked 'var', remove it.
      unsigned varKind = isa<ParamDecl>(var);
      if (fixItLoc.isInvalid()) {
        diags.diagnose(var->getLoc(), diag::variable_never_mutated,
                       var->getName(), varKind, true);
      } else {
        bool suggestLet = true;
        if (auto *stmt = var->getParentPatternStmt()) {
          // Don't try to suggest 'var' -> 'let' conversion
          // in case of 'for' loop because it's an implicitly
          // immutable context.
          suggestLet = !isa<ForEachStmt>(stmt);
        }

        auto diag = diags.diagnose(var->getLoc(), diag::variable_never_mutated,
                                   var->getName(), varKind, suggestLet);

        if (suggestLet)
          diag.fixItReplace(fixItLoc, "let");
        else
          diag.fixItRemove(fixItLoc);

        continue;
      }
    }

    // If this is a variable that was only written to, emit a warning.
    if ((access & RK_Read) == 0) {
      diags.diagnose(var->getLoc(), diag::variable_never_read, var->getName(),
                     isa<ParamDecl>(var));
      continue;
    }
  }
}

/// Handle a store to "x.y" where 'base' is the expression for x and 'decl' is
/// the decl for 'y'.
void VarDeclUsageChecker::markBaseOfAbstractStorageDeclStore(
    Expr *base, ConcreteDeclRef decl) {
  // If the base is a class or an rvalue, then this store just loads the base.
  if (base->getType()->isAnyClassReferenceType() ||
      !(base->getType()->hasLValueType() || base->isSemanticallyInOutExpr())) {
    base->walk(*this);
    return;
  }

  // If the store is to a non-mutating member, then this is just a load, even
  // if the base is an inout expr.
  auto *asd = cast<AbstractStorageDecl>(decl.getDecl());
  if (asd->isSettable(nullptr) && !asd->isSetterMutating()) {
    // Sema conservatively converts the base to inout expr when it is an lvalue.
    // Look through it because we know it isn't actually doing a load/store.
    if (auto *ioe = dyn_cast<InOutExpr>(base))
      base = ioe->getSubExpr();
    base->walk(*this);
    return;
  }

  // Otherwise this is a read and write of the base.
  return markStoredOrInOutExpr(base, RK_Written | RK_Read);
}

void VarDeclUsageChecker::markStoredOrInOutExpr(Expr *e, unsigned flags) {
  // Sema leaves some subexpressions null, which seems really unfortunate.  It
  // should replace them with ErrorExpr.
  if (e == nullptr || !e->getType() || e->getType()->hasError()) {
    sawError = true;
    return;
  }

  // Ignore parens and other easy cases.
  e = e->getSemanticsProvidingExpr();

  // If we found a decl that is being assigned to, then mark it.
  if (auto *dre = dyn_cast<DeclRefExpr>(e)) {
    addMark(dre->getDecl(), flags);
    return;
  }

  if (auto *te = dyn_cast<TupleExpr>(e)) {
    for (auto &elt : te->getElements())
      markStoredOrInOutExpr(elt, flags);
    return;
  }

  // If this is an assignment into a mutating subscript lvalue expr, then we
  // are mutating the base expression.  We also need to visit the index
  // expressions as loads though.
  if (auto *se = dyn_cast<SubscriptExpr>(e)) {
    // The index of the subscript is evaluated as an rvalue.
    se->getIndex()->walk(*this);
    if (se->hasDecl())
      markBaseOfAbstractStorageDeclStore(se->getBase(), se->getDecl());
    else // FIXME: Should not be needed!
      markStoredOrInOutExpr(se->getBase(), RK_Written | RK_Read);

    return;
  }

  // Likewise for key path applications. An application of a WritableKeyPath
  // reads and writes its base.
  if (auto *kpa = dyn_cast<KeyPathApplicationExpr>(e)) {
    auto &ctx = kpa->getType()->getASTContext();
    kpa->getKeyPath()->walk(*this);
    if (kpa->getKeyPath()->getType()->getAnyNominal() ==
        ctx.getWritableKeyPathDecl())
      markStoredOrInOutExpr(kpa->getBase(), RK_Written | RK_Read);
    if (kpa->getKeyPath()->getType()->getAnyNominal() ==
        ctx.getReferenceWritableKeyPathDecl())
      markStoredOrInOutExpr(kpa->getBase(), RK_Read);
    return;
  }

  if (auto *ioe = dyn_cast<InOutExpr>(e))
    return markStoredOrInOutExpr(ioe->getSubExpr(), RK_Written | RK_Read);

  if (auto *mre = dyn_cast<MemberRefExpr>(e)) {
    markBaseOfAbstractStorageDeclStore(mre->getBase(), mre->getMember());
    return;
  }

  if (auto *tee = dyn_cast<TupleElementExpr>(e))
    return markStoredOrInOutExpr(tee->getBase(), flags);

  if (auto *fve = dyn_cast<ForceValueExpr>(e))
    return markStoredOrInOutExpr(fve->getSubExpr(), flags);

  if (auto *boe = dyn_cast<BindOptionalExpr>(e))
    return markStoredOrInOutExpr(boe->getSubExpr(), flags);

  // If this is an OpaqueValueExpr that we've seen a mapping for, jump to the
  // mapped value.
  if (auto *ove = dyn_cast<OpaqueValueExpr>(e))
    if (auto *expr = opaqueValueMap[ove])
      return markStoredOrInOutExpr(expr, flags);

  // If we don't know what kind of expression this is, assume it's a reference
  // and mark it as a read.
  e->walk(*this);
}

/// The heavy lifting happens when visiting expressions.
std::pair<bool, Expr *> VarDeclUsageChecker::walkToExprPre(Expr *e) {
  // Sema leaves some subexpressions null, which seems really unfortunate.  It
  // should replace them with ErrorExpr.
  if (e == nullptr || !e->getType() || e->getType()->hasError()) {
    sawError = true;
    return {false, e};
  }

  // If this is a DeclRefExpr found in a random place, it is a load of the
  // vardecl.
  if (auto *dre = dyn_cast<DeclRefExpr>(e)) {
    addMark(dre->getDecl(), RK_Read);

    // If the Expression is a read of a getter, track for diagnostics
    if (auto *vd = dyn_cast<VarDecl>(dre->getDecl())) {
      if (associatedGetter == vd && associatedGetterRefExpr == nullptr)
        associatedGetterRefExpr = dre;
    }
  }
  // If the Expression is a member reference, see if it is a read of the getter
  // to track for diagnostics.
  if (auto *mre = dyn_cast<MemberRefExpr>(e)) {
    if (auto *vd = dyn_cast<VarDecl>(mre->getMember().getDecl())) {
      if (associatedGetter == vd && associatedGetterRefExpr == nullptr)
        associatedGetterRefExpr = mre;
    }
  }
  // If this is an AssignExpr, see if we're mutating something that we know
  // about.
  if (auto *assign = dyn_cast<AssignExpr>(e)) {
    markStoredOrInOutExpr(assign->getDest(), RK_Written);

    // Don't walk into the LHS of the assignment, only the RHS.
    assign->getSrc()->walk(*this);
    return {false, e};
  }

  // '&x' is a read and write of 'x'.
  if (auto *io = dyn_cast<InOutExpr>(e)) {
    markStoredOrInOutExpr(io->getSubExpr(), RK_Read | RK_Written);
    // Don't bother walking into this.
    return {false, e};
  }

  // If we see an OpenExistentialExpr, remember the mapping for its OpaqueValue.
  if (auto *oee = dyn_cast<OpenExistentialExpr>(e))
    opaqueValueMap[oee->getOpaqueValue()] = oee->getExistentialValue();

  // If we saw an ErrorExpr, take note of this.
  if (isa<ErrorExpr>(e))
    sawError = true;

  return {true, e};
}

/// handle #if directives.  All of the active clauses are already walked by the
/// AST walker, but we also want to handle the inactive ones to avoid false
/// positives.
void VarDeclUsageChecker::handleIfConfig(IfConfigDecl *icd) {
  struct ConservativeDeclMarker : public ASTWalker {
    VarDeclUsageChecker &vduc;
    ConservativeDeclMarker(VarDeclUsageChecker &vduc) : vduc(vduc) {}

    Expr *walkToExprPost(Expr *e) override {
      // If we see a bound reference to a decl in an inactive #if block, then
      // conservatively mark it read and written.  This will silence "variable
      // unused" and "could be marked let" warnings for it.
      if (auto *dre = dyn_cast<DeclRefExpr>(e))
        vduc.addMark(dre->getDecl(), RK_Read | RK_Written);
      return e;
    }
  };

  for (auto &clause : icd->getClauses()) {
    // Active clauses are handled by the normal AST walk.
    if (clause.isActive)
      continue;

    for (auto elt : clause.Elements)
      elt.walk(ConservativeDeclMarker(*this));
  }
}

//===----------------------------------------------------------------------===//
//                           Top Level Entrypoints
//===----------------------------------------------------------------------===//

/// Apply the warnings managed by VarDeclUsageChecker to the top level
/// code declarations that haven't been checked yet.
void swift::performTopLevelDeclDiagnostics(TypeChecker &tc,
                                           TopLevelCodeDecl *tlcd) {
  VarDeclUsageChecker checker(tc.Diags);
  tlcd->walk(checker);
}

/// Perform diagnostics for func/init/deinit declarations.
void swift::performAbstractFuncDeclDiagnostics(TypeChecker &tc,
                                               AbstractFunctionDecl *afd) {
  assert(afd->getBody() && "Need a body to check");

  // Don't produce these diagnostics for implicitly generated code.
  if (afd->getLoc().isInvalid() || afd->isImplicit() || afd->isInvalid())
    return;

  // Check for unused variables, as well as variables that are could be
  // declared as constants.
  afd->getBody()->walk(VarDeclUsageChecker(tc, afd));
}
