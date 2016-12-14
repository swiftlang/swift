//===--- ConditionResolver.cpp - Condition Resolution ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements condition resolution for Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTWalker.h"
#include "swift/Parse/Parser.h"
#include "llvm/ADT/TinyPtrVector.h"

using namespace swift;

namespace {
  class ConditionClauseResolver : public StmtTransformer {
  public:
    // A declaration is "notable" if we must
    // 1) Warn about it if the user has not imported Foundation
    // 2) Insert it into the local type declaration list
    class NotableDeclFinder : public ASTWalker {
      SourceFile &SF;
    public:
      NotableDeclFinder(SourceFile &SF) : SF(SF) {}

      bool walkToDeclPre(Decl *D) override {
        if (D->isImplicit()) return false;
        if (isa<DestructorDecl>(D)) return false;

        for (auto Attr : D->getAttrs()) {
          if (Attr->isImplicit()) continue;
          if (isa<ObjCAttr>(Attr) || isa<DynamicAttr>(Attr))
            SF.AttrsRequiringFoundation.insert(Attr);
        }

        if (D->getDeclContext()->isLocalContext()) {
          if (auto tyDecl = dyn_cast<TypeDecl>(D)){
            if (!isa<TypeAliasDecl>(tyDecl))
              SF.LocalTypeDecls.insert(tyDecl);
          }
        }
        return true;
      }
    };
    NotableDeclFinder NDF;
    SmallVectorImpl<Decl *> &ExtraTLCDs;
    SourceFile &SF;
    ASTContext &Context;

    ConditionClauseResolver(SmallVectorImpl<Decl *> &ExtraTLCDs,
                            SourceFile &SF)
    : NDF(SF), ExtraTLCDs(ExtraTLCDs), SF(SF),
    Context(SF.getASTContext()) {}

    void resolveClausesAndInsertMembers(IterableDeclContext *Nom,
                                        IfConfigDecl *Cond) {
      if (Cond->isResolved()) return;
      for (auto &clause : Cond->getClauses()) {
        // Evaluate conditions until we find the active clause.
        DiagnosticTransaction DT(Context.Diags);
        auto classification =
        Parser::classifyConditionalCompilationExpr(clause.Cond, Context,
                                                   Context.Diags,
                                                   /*fullCheck*/ true);
        DT.abort();
        if (clause.Cond && !classification.getValueOr(false)) {
          continue;
        }

        for (auto &member : clause.Members) {
          if (auto innerConfig = dyn_cast<IfConfigDecl>(member)) {
            resolveClausesAndInsertMembers(Nom, innerConfig);
          } else {
            Nom->addMember(member);
          }

          // For newly-inserted aggregates and extensions we need to warn
          // if they're marked '@objc'.
          member->walk(NDF);
        }

        // Now that we've evaluated the active clause, we no longer need to
        // look into further clauses.
        break;
      }
      Cond->setResolved();
    }

    BraceStmt *transformBraceStmt(BraceStmt *BS, bool TopLevel) override {
      SmallVector<ASTNode, 16> Elements;
      bool didEvaluateCondition = false;
      for (const auto &elt : BS->getElements()) {
        // If this is a declaration we may have to insert it into the local
        // type decl list.
        if (Decl *D = elt.dyn_cast<Decl *>()) {
          // Dig for, and warn about, decls that require Foundation.
          D->walk(NDF);
          if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
            for (unsigned i = 0, e = PBD->getNumPatternEntries(); i != e; ++i) {
              if (auto e = PBD->getInit(i)) {
                PBD->setInit(i, e->walk(CF));
              }
            }
          }
          Elements.push_back(elt);
          continue;
        }

        // If this is an expression we have to walk it to transform any interior
        // closure expression bodies.
        if (Expr *E = elt.dyn_cast<Expr *>()) {
          Elements.push_back(E->walk(CF));
          continue;
        }

        // We only transform statements.
        Stmt *stmt = elt.dyn_cast<Stmt *>();
        assert(stmt && "Unhandled ASTNode Kind?");
        (void)stmt;

        // Build configurations are handled later.  If this isn't a build
        // configuration transform the statement only.
        IfConfigStmt *config = dyn_cast<IfConfigStmt>(stmt);
        if (!config) {
          Elements.push_back(transformStmt(stmt));
          continue;
        }

        // Insert the configuration and evaluate its clauses if we haven't seen
        // it before.
        Elements.push_back(config);
        if (config->isResolved()) continue;
        didEvaluateCondition = true;
        for (auto &clause : config->getClauses()) {
          // Evaluate conditions until we find the active clause.
          DiagnosticTransaction DT(Context.Diags);
          auto classification =
            Parser::classifyConditionalCompilationExpr(clause.Cond, Context,
                                                       Context.Diags,
                                                       /*fullCheck*/ true);
          DT.abort();
          if (clause.Cond && !classification.getValueOr(false)) {
            continue;
          }

          // Now that we have the active clause, look into its elements for
          // further statements to transform.
          for (auto &clauseElt : clause.Elements) {
            // If this is a declaration we may have to insert it into the local
            // type decl list.
            if (Decl *D = clauseElt.dyn_cast<Decl *>()) {
              // Nested top-level code declarations are a special case that
              // require extra transformation.
              if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
                if (TLCD->isImplicit()) {
                  continue;
                }

                if (BraceStmt *Body = TLCD->getBody()) {
                  BraceStmt *NewBody = transformBraceStmt(Body, true);
                  if (NewBody != Body) {
                    TLCD->setBody(NewBody);
                  }
                }
                ExtraTLCDs.push_back(TLCD);
              } else {
                // For newly-inserted aggregates and extensions we need to warn
                // if they're marked '@objc'.
                D->walk(NDF);

                if (TopLevel) {
                  ExtraTLCDs.push_back(D);
                } else {
                  Elements.push_back(D);
                }
              }
              continue;
            }

            auto innerStmt = clauseElt.dyn_cast<Stmt *>();
            if (!innerStmt) {
              Elements.push_back(clauseElt);
              continue;
            }

            auto innerConfig = dyn_cast<IfConfigStmt>(innerStmt);
            if (!innerConfig) {
              Elements.push_back(transformStmt(innerStmt));
              continue;
            }

            // Nested build configurations require an additional transform to
            // evaluate completely.
            auto BS = BraceStmt::create(Context, SourceLoc(),
                                        {innerConfig}, SourceLoc());
            for (auto &es : transformBraceStmt(BS, TopLevel)->getElements()) {
              if (auto innerInnerStmt = es.dyn_cast<Stmt *>()) {
                Elements.push_back(transformStmt(innerInnerStmt));
              } else {
                Elements.push_back(es);
              }
            }
          }

          // Now that we've evaluated the active clause, we no longer need to
          // look into further clauses.
          break;
        }
        config->setResolved();
      }
      // If we didn't have to evaluate any condition clauses, the brace
      // statement does not require reallocation.
      if (!didEvaluateCondition) {
        return BS;
      }
      return BraceStmt::create(Context, BS->getStartLoc(),
                               Context.AllocateCopy(Elements),
                               BS->getEndLoc());
    }
  };

  class ConditionClauseWalker : public ASTWalker {
  public:
    ConditionClauseResolver R;
    ConditionClauseWalker(SmallVectorImpl<Decl *> &ExtraTLCDs,
                          SourceFile &SF)
    : R(ExtraTLCDs, SF) {}

    bool walkToDeclPre(Decl *D) override {
      if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
        // For top-level declarations in libraries or in the interpreter we
        // extract and evaluate any conditions and their corresponding blocks.
        if (TLCD->isImplicit()) {
          return false;
        }

        if (BraceStmt *Body = TLCD->getBody()) {
          BraceStmt *NewBody = R.transformBraceStmt(Body, true);
          if (NewBody != Body) {
            TLCD->setBody(NewBody);
          }
        }
      } else if (AbstractFunctionDecl *FD = dyn_cast<AbstractFunctionDecl>(D)) {
        // For functions, transform the body.
        if (FD->isImplicit()) {
          return false;
        }

        // Take care not to force the body of delayed functions.
        auto Body = FD->getBody();
        if (!Body) {
          return false;
        }

        BraceStmt *NewBody = R.transformBraceStmt(Body, false);
        if (NewBody != Body) {
          FD->setBody(NewBody);
        }
      } else if (auto *IDC = dyn_cast<IterableDeclContext>(D)) {
        // Dig for, and warn about, decls that require Foundation.
        D->walk(R.NDF);

        // For aggregates and extensions, gather members under the scope of
        // build conditions and insert them if they're active.
        SmallPtrSet<IfConfigDecl *, 4> configsToResolve;
        for (auto member : IDC->getMembers()) {
          auto condition = dyn_cast<IfConfigDecl>(member);
          if (!condition)
            continue;
          configsToResolve.insert(condition);
        }
        for (auto condition : configsToResolve) {
          R.resolveClausesAndInsertMembers(IDC, condition);
        }
      }
      return true;
    }
  };
}

//===----------------------------------------------------------------------===//
// performConditionResolution
//===----------------------------------------------------------------------===//

static Decl *resolveConditionalClauses(Decl *TLD, SourceFile &File,
                                       SmallVectorImpl<Decl *> &ExtraTLCD) {
  ConditionClauseWalker CCR(ExtraTLCD, File);
  TLD->walk(CCR);
  return TLD;
}

void swift::performDelayedConditionResolution(Decl *D, SourceFile &BSF,
                                              SmallVectorImpl<Decl *> &Extras) {
  resolveConditionalClauses(D, BSF, Extras);
}

void swift::performConditionResolution(SourceFile &SF) {
  // Preprocess the declarations to evaluate compilation condition clauses.
  SmallVector<Decl *, 8> resolvedDecls;
  for (auto *decl : SF.Decls) {
    // This first pass will unpack any top level declarations.
    // The next will resolve any further conditions in these
    // new top level declarations.
    resolvedDecls.push_back(decl);
    auto idx = resolvedDecls.size();
    resolvedDecls[idx - 1] = resolveConditionalClauses(decl, SF, resolvedDecls);
  }
  SF.Decls.clear();
  for (auto &d : resolvedDecls) {
    if (!SF.isScriptMode()) {
      if (auto TLCD = dyn_cast<TopLevelCodeDecl>(d)) {
        // When parsing library files, skip empty top level code declarations.
        auto elements = TLCD->getBody()->getElements();
        if (elements.empty()) continue;
        // These are artifacts of the condition clause transform.
        auto stmt = elements.front().dyn_cast<Stmt *>();
        if ((elements.size() == 1) && stmt && isa<IfConfigStmt>(stmt)) continue;
      }
    }
    SmallVector<Decl *, 2> scratch;
    auto processedD = resolveConditionalClauses(d, SF, scratch);
    assert(scratch.empty());
    SF.Decls.push_back(processedD);
  }
}
