//===--- NameBinding.cpp - Name Binding -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements name binding for Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/AST.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/Parse/Parser.h"
#include "swift/ClangImporter/ClangModule.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/Path.h"
#include <algorithm>
#include <system_error>
using namespace swift;

//===----------------------------------------------------------------------===//
// NameBinder
//===----------------------------------------------------------------------===//

using ImportedModule = Module::ImportedModule;
using ImportOptions = SourceFile::ImportOptions;

namespace {  
  class NameBinder {    
  public:
    SourceFile &SF;
    ASTContext &Context;

    NameBinder(SourceFile &SF) : SF(SF), Context(SF.getASTContext()) {}

    template<typename ...ArgTypes>
    InFlightDiagnostic diagnose(ArgTypes &&...Args) {
      return Context.Diags.diagnose(std::forward<ArgTypes>(Args)...);
    }
    
    void addImport(
        SmallVectorImpl<std::pair<ImportedModule, ImportOptions>> &imports,
        ImportDecl *ID);

    /// Load a module referenced by an import statement.
    ///
    /// Returns null if no module can be loaded.
    Module *getModule(ArrayRef<std::pair<Identifier,SourceLoc>> ModuleID);
  };
}

Module *
NameBinder::getModule(ArrayRef<std::pair<Identifier, SourceLoc>> modulePath) {
  assert(!modulePath.empty());
  auto moduleID = modulePath[0];
  
  // The Builtin module cannot be explicitly imported unless we're a .sil file
  // or in the REPL.
  if ((SF.Kind == SourceFileKind::SIL || SF.Kind == SourceFileKind::REPL) &&
      moduleID.first == Context.TheBuiltinModule->getName())
    return Context.TheBuiltinModule;

  // If the imported module name is the same as the current module,
  // skip the Swift module loader and use the Clang module loader instead.
  // This allows a Swift module to extend a Clang module of the same name.
  //
  // FIXME: We'd like to only use this in SIL mode, but unfortunately we use it
  // for our fake overlays as well.
  if (moduleID.first == SF.getParentModule()->getName() &&
      modulePath.size() == 1) {
    if (auto importer = Context.getClangModuleLoader())
      return importer->loadModule(moduleID.second, modulePath);
    return nullptr;
  }
  
  return Context.getModule(modulePath);
}

/// Returns true if a decl with the given \p actual kind can legally be
/// imported via the given \p expected kind.
static bool isCompatibleImportKind(ImportKind expected, ImportKind actual) {
  if (expected == actual)
    return true;
  if (expected != ImportKind::Type)
    return false;

  switch (actual) {
  case ImportKind::Module:
    llvm_unreachable("module imports do not bring in decls");
  case ImportKind::Type:
    llvm_unreachable("individual decls cannot have abstract import kind");
  case ImportKind::Struct:
  case ImportKind::Class:
  case ImportKind::Enum:
    return true;
  case ImportKind::Protocol:
  case ImportKind::Var:
  case ImportKind::Func:
    return false;
  }

  llvm_unreachable("Unhandled ImportKind in switch.");
}

static const char *getImportKindString(ImportKind kind) {
  switch (kind) {
  case ImportKind::Module:
    llvm_unreachable("module imports do not bring in decls");
  case ImportKind::Type:
    return "typealias";
  case ImportKind::Struct:
    return "struct";
  case ImportKind::Class:
    return "class";
  case ImportKind::Enum:
    return "enum";
  case ImportKind::Protocol:
    return "protocol";
  case ImportKind::Var:
    return "var";
  case ImportKind::Func:
    return "func";
  }

  llvm_unreachable("Unhandled ImportKind in switch.");
}

static bool shouldImportSelfImportClang(const ImportDecl *ID,
                                        const SourceFile &SF) {
  // FIXME: We use '@_exported' for fake overlays in testing.
  if (ID->isExported())
    return true;
  if (SF.Kind == SourceFileKind::SIL)
    return true;
  return false;
}

void NameBinder::addImport(
    SmallVectorImpl<std::pair<ImportedModule, ImportOptions>> &imports,
    ImportDecl *ID) {
  if (ID->getModulePath().front().first == SF.getParentModule()->getName() &&
      ID->getModulePath().size() == 1 && !shouldImportSelfImportClang(ID, SF)) {
    // If the imported module name is the same as the current module,
    // produce a diagnostic.
    StringRef filename = llvm::sys::path::filename(SF.getFilename());
    if (filename.empty())
      Context.Diags.diagnose(ID, diag::sema_import_current_module,
                             ID->getModulePath().front().first);
    else
      Context.Diags.diagnose(ID, diag::sema_import_current_module_with_file,
                             filename, ID->getModulePath().front().first);
    ID->setModule(SF.getParentModule());
    return;
  }

  Module *M = getModule(ID->getModulePath());
  if (!M) {
    SmallString<64> modulePathStr;
    interleave(ID->getModulePath(),
               [&](ImportDecl::AccessPathElement elem) {
                 modulePathStr += elem.first.str();
               },
               [&] { modulePathStr += "."; });

    auto diagKind = diag::sema_no_import;
    if (SF.Kind == SourceFileKind::REPL || Context.LangOpts.DebuggerSupport)
      diagKind = diag::sema_no_import_repl;
    diagnose(ID->getLoc(), diagKind, modulePathStr);

    if (Context.SearchPathOpts.SDKPath.empty() &&
        llvm::Triple(llvm::sys::getProcessTriple()).isMacOSX()) {
      diagnose(SourceLoc(), diag::sema_no_import_no_sdk);
      diagnose(SourceLoc(), diag::sema_no_import_no_sdk_xcrun);
    }
    return;
  }

  ID->setModule(M);

  Module *topLevelModule;
  if (ID->getModulePath().size() == 1) {
    topLevelModule = M;
  } else {
    // If we imported a submodule, import the top-level module as well.
    Identifier topLevelName = ID->getModulePath().front().first;
    topLevelModule = Context.getLoadedModule(topLevelName);
    assert(topLevelModule && "top-level module missing");
  }

  auto *testableAttr = ID->getAttrs().getAttribute<TestableAttr>();
  if (testableAttr && !topLevelModule->isTestingEnabled() &&
      Context.LangOpts.EnableTestableAttrRequiresTestableModule) {
    diagnose(ID->getModulePath().front().second, diag::module_not_testable,
             topLevelModule->getName());
    testableAttr->setInvalid();
  }

  ImportOptions options;
  if (ID->isExported())
    options |= SourceFile::ImportFlags::Exported;
  if (testableAttr)
    options |= SourceFile::ImportFlags::Testable;
  imports.push_back({ { ID->getDeclPath(), M }, options });

  if (topLevelModule != M)
    imports.push_back({ { ID->getDeclPath(), topLevelModule }, options });

  if (ID->getImportKind() != ImportKind::Module) {
    // If we're importing a specific decl, validate the import kind.
    using namespace namelookup;
    auto declPath = ID->getDeclPath();

    // FIXME: Doesn't handle scoped testable imports correctly.
    assert(declPath.size() == 1 && "can't handle sub-decl imports");
    SmallVector<ValueDecl *, 8> decls;
    lookupInModule(topLevelModule, declPath, declPath.front().first, decls,
                   NLKind::QualifiedLookup, ResolutionKind::Overloadable,
                   /*resolver*/nullptr, &SF);

    if (decls.empty()) {
      diagnose(ID, diag::decl_does_not_exist_in_module,
               static_cast<unsigned>(ID->getImportKind()),
               declPath.front().first,
               ID->getModulePath().front().first)
        .highlight(SourceRange(declPath.front().second,
                               declPath.back().second));
      return;
    }

    ID->setDecls(Context.AllocateCopy(decls));

    Optional<ImportKind> actualKind = ImportDecl::findBestImportKind(decls);
    if (!actualKind.hasValue()) {
      // FIXME: print entire module name?
      diagnose(ID, diag::ambiguous_decl_in_module,
               declPath.front().first, M->getName());
      for (auto next : decls)
        diagnose(next, diag::found_candidate);

    } else if (!isCompatibleImportKind(ID->getImportKind(), *actualKind)) {
      diagnose(ID, diag::imported_decl_is_wrong_kind,
               declPath.front().first,
               getImportKindString(ID->getImportKind()),
               static_cast<unsigned>(*actualKind))
        .fixItReplace(SourceRange(ID->getKindLoc()),
                      getImportKindString(*actualKind));

      if (decls.size() == 1)
        diagnose(decls.front(), diag::decl_declared_here,
                 decls.front()->getFullName());
    }
  }
}

//===----------------------------------------------------------------------===//
// performNameBinding
//===----------------------------------------------------------------------===//

template<typename OP_DECL>
static void insertOperatorDecl(NameBinder &Binder,
                               SourceFile::OperatorMap<OP_DECL*> &Operators,
                               OP_DECL *OpDecl) {
  auto previousDecl = Operators.find(OpDecl->getName());
  if (previousDecl != Operators.end()) {
    Binder.diagnose(OpDecl->getLoc(), diag::operator_redeclared);
    Binder.diagnose(previousDecl->second.getPointer(),
                    diag::previous_operator_decl);
    return;
  }

  // FIXME: The second argument indicates whether the given operator is visible
  // outside the current file.
  Operators[OpDecl->getName()] = { OpDecl, true };
}

static void insertPrecedenceGroupDecl(NameBinder &binder, SourceFile &SF,
                                      PrecedenceGroupDecl *group) {
  auto previousDecl = SF.PrecedenceGroups.find(group->getName());
  if (previousDecl != SF.PrecedenceGroups.end()) {
    binder.diagnose(group->getLoc(), diag::precedence_group_redeclared);
    binder.diagnose(previousDecl->second.getPointer(),
                    diag::previous_precedence_group_decl);
    return;
  }

  // FIXME: The second argument indicates whether the given precedence
  // group is visible outside the current file.
  SF.PrecedenceGroups[group->getName()] = { group, true };  
}

static Decl *resolveConditionalClauses(Decl *TLD, NameBinder &binder,
                                       SmallVectorImpl<Decl *> &ExtraTLCD) {

  class ConditionClauseWalker : public ASTWalker {
    class ConditionClauseResolver : public StmtTransformer {
    public:
      // A declaration is "notable" if we must
      // 1) Warn about it if the user has not imported Foundation
      // 2) Insert it into the local type declaration list
      class NotableDeclFinder : public ASTWalker {
        NameBinder &Binder;
      public:
        NotableDeclFinder(NameBinder &Binder) : Binder(Binder) {}

        bool walkToDeclPre(Decl *D) override {
          if (D->isImplicit()) return false;
          if (isa<DestructorDecl>(D)) return false;
          
          for (auto Attr : D->getAttrs()) {
            if (Attr->isImplicit()) continue;
            if (isa<ObjCAttr>(Attr) || isa<DynamicAttr>(Attr))
              Binder.SF.AttrsRequiringFoundation.insert(Attr);
          }

          if (D->getDeclContext()->isLocalContext()) {
            if (auto tyDecl = dyn_cast<TypeDecl>(D)){
              if (!isa<TypeAliasDecl>(tyDecl))
                Binder.SF.LocalTypeDecls.insert(tyDecl);
            }
          }
          return true;
        }
      };
      NotableDeclFinder NDF;
      SmallVectorImpl<Decl *> &ExtraTLCDs;
      NameBinder &Binder;

      ConditionClauseResolver(SmallVectorImpl<Decl *> &ExtraTLCDs,
                              NameBinder &Binder)
        : NDF(Binder), ExtraTLCDs(ExtraTLCDs), Binder(Binder) {}

    void resolveClausesAndInsertMembers(IterableDeclContext *Nom,
                                        IfConfigDecl *Cond) {
      if (Cond->isResolved()) return;
      for (auto &clause : Cond->getClauses()) {
        // Evaluate conditions until we find the active clause.
        DiagnosticTransaction DT(Binder.Context.Diags);
        auto classification =
          Parser::classifyConditionalCompilationExpr(clause.Cond,
                                                     Binder.Context,
                                                     Binder.Context.Diags,
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
      ASTContext &Context = Binder.Context;
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
          DiagnosticTransaction DT(Binder.Context.Diags);
          auto classification =
            Parser::classifyConditionalCompilationExpr(clause.Cond,
                                                       Binder.Context,
                                                       Binder.Context.Diags,
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

  public:
    ConditionClauseResolver R;
    NameBinder &Binder;
    ConditionClauseWalker(SmallVectorImpl<Decl *> &ExtraTLCDs,
                          NameBinder &Binder)
      : R(ExtraTLCDs, Binder), Binder(Binder) {}

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
        // For functions, transform the statements in the body.
        // FIXME: This can be deferred until type checking.
        if (FD->isImplicit()) {
          return false;
        }
        
        if (BraceStmt *Body = FD->getBody()) {
          BraceStmt *NewBody = R.transformBraceStmt(Body, false);
          if (NewBody != Body) {
            FD->setBody(NewBody);
          }
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
  
  ConditionClauseWalker CCR(ExtraTLCD, binder);
  TLD->walk(CCR);
  return TLD;
}

/// performNameBinding - Once parsing is complete, this walks the AST to
/// resolve names and do other top-level validation.
///
/// At this parsing has been performed, but we still have UnresolvedDeclRefExpr
/// nodes for unresolved value names, and we may have unresolved type names as
/// well.  This handles import directives and forward references.
void swift::performNameBinding(SourceFile &SF, unsigned StartElem) {
  // Make sure we skip adding the standard library imports if the
  // source file is empty.
  if (SF.ASTStage == SourceFile::NameBound || SF.Decls.empty()) {
    SF.ASTStage = SourceFile::NameBound;
    return;
  }

  // Reset the name lookup cache so we find new decls.
  // FIXME: This is inefficient.
  SF.clearLookupCache();

  NameBinder Binder(SF);

  // Preprocess the declarations to evaluate compilation condition clauses.
  SmallVector<Decl *, 8> resolvedDecls;
  std::for_each(SF.Decls.begin(), SF.Decls.end(),
                [&](Decl *decl) {
                  // This first pass will unpack any top level declarations.
                  // The next will resolve any further conditions in these
                  // new top level declarations.
                  resolvedDecls.push_back(decl);
                  auto idx = resolvedDecls.size();
                  resolvedDecls[idx - 1]
                    = resolveConditionalClauses(decl, Binder, resolvedDecls);
                });
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
    SmallVector<Decl *, 8> scratch;
    auto processedD = resolveConditionalClauses(d, Binder, scratch);
    assert(scratch.empty());
    SF.Decls.push_back(processedD);
  }

  SmallVector<std::pair<ImportedModule, ImportOptions>, 8> ImportedModules;

  // Do a prepass over the declarations to find and load the imported modules
  // and map operator decls.
  for (auto D : llvm::makeArrayRef(SF.Decls).slice(StartElem)) {
    if (ImportDecl *ID = dyn_cast<ImportDecl>(D)) {
      Binder.addImport(ImportedModules, ID);
    } else if (auto *OD = dyn_cast<PrefixOperatorDecl>(D)) {
      insertOperatorDecl(Binder, SF.PrefixOperators, OD);
    } else if (auto *OD = dyn_cast<PostfixOperatorDecl>(D)) {
      insertOperatorDecl(Binder, SF.PostfixOperators, OD);
    } else if (auto *OD = dyn_cast<InfixOperatorDecl>(D)) {
      insertOperatorDecl(Binder, SF.InfixOperators, OD);
    } else if (auto *PGD = dyn_cast<PrecedenceGroupDecl>(D)) {
      insertPrecedenceGroupDecl(Binder, SF, PGD);
    }
  }

  SF.addImports(ImportedModules);

  // FIXME: This algorithm has quadratic memory usage.  (In practice,
  // import statements after the first "chunk" should be rare, though.)
  // FIXME: Can we make this more efficient?

  SF.ASTStage = SourceFile::NameBound;
  verify(SF);
}

