//===--- ASTScriptEvaluator.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// AST script evaluation.
///
//===----------------------------------------------------------------------===//

#include "ASTScript.h"
#include "ASTScriptConfiguration.h"

#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/Frontend/Frontend.h"

using namespace swift;
using namespace scripting;

namespace {

class ASTScriptWalker : public ASTWalker {
  const ASTScript &Script;
  ProtocolDecl *ViewProtocol;

public:
  ASTScriptWalker(const ASTScript &script, ProtocolDecl *viewProtocol)
    : Script(script), ViewProtocol(viewProtocol) {}

  bool walkToDeclPre(Decl *D) override {
    visit(D);
    return true;
  }

  void visit(const Decl *D) {
    auto fn = dyn_cast<AbstractFunctionDecl>(D);
    if (!fn) return;

    // Suppress warnings.
    (void) Script;

    for (auto param : *fn->getParameters()) {
      // The parameter must have function type.
      auto paramType = param->getInterfaceType();
      auto paramFnType = paramType->getAs<FunctionType>();
      if (!paramFnType) continue;

      // The parameter function must return a type parameter that
      // conforms to SwiftUI.View.
      auto paramResultType = paramFnType->getResult();
      if (!paramResultType->isTypeParameter()) continue;
      auto sig = fn->getGenericSignature();
      if (!sig->conformsToProtocol(paramResultType, ViewProtocol)) continue;

      // The parameter must not be a @ViewBuilder parameter.
      if (param->getFunctionBuilderType()) continue;

      // Print the function.
      printDecl(fn);
    }
  }

  void printDecl(const ValueDecl *decl) {
    // FIXME: there's got to be some better way to print an exact reference
    // to a declaration, including its context.
    printDecl(llvm::outs(), decl);
    llvm::outs() << "\n";
  }

  void printDecl(llvm::raw_ostream &out, const ValueDecl *decl) {
    if (auto accessor = dyn_cast<AccessorDecl>(decl)) {
      printDecl(out, accessor->getStorage());
      out << ".(accessor)";
    } else {
      printDeclContext(out, decl->getDeclContext());
      out << decl->getFullName();
    }
  }

  void printDeclContext(llvm::raw_ostream &out, const DeclContext *dc) {
    if (!dc) return;
    if (auto module = dyn_cast<ModuleDecl>(dc)) {
      out << module->getName() << ".";
    } else if (auto extension = dyn_cast<ExtensionDecl>(dc)) {
      printDecl(out, extension->getExtendedNominal());
      out << ".";
    } else if (auto decl = dyn_cast_or_null<ValueDecl>(dc->getAsDecl())) {
      printDecl(out, decl);
      out << ".";
    } else {
      printDeclContext(out, dc->getParent());
    }
  }
};

}

bool ASTScript::execute() const {
  // Hardcode the actual query we want to execute here.

  auto &ctx = Config.Compiler.getASTContext();
  auto swiftUI = ctx.getLoadedModule(ctx.getIdentifier("SwiftUI"));
  if (!swiftUI) {
    llvm::errs() << "error: SwiftUI module not loaded\n";
    return true;
  }

  auto descriptor =
      UnqualifiedLookupDescriptor(DeclNameRef(ctx.getIdentifier("View")),
                                  swiftUI);
  auto viewLookup = evaluateOrDefault(ctx.evaluator,
                                      UnqualifiedLookupRequest{descriptor}, {});
  auto viewProtocol =
    dyn_cast_or_null<ProtocolDecl>(viewLookup.getSingleTypeResult());
  if (!viewProtocol) {
    llvm::errs() << "error: couldn't find SwiftUI.View protocol\n";
    return true;
  }

  SmallVector<Decl*, 128> topLevelDecls;
  swiftUI->getTopLevelDecls(topLevelDecls);

  llvm::errs() << "found " << topLevelDecls.size() << " top-level declarations\n";

  ASTScriptWalker walker(*this, viewProtocol);
  for (auto decl : topLevelDecls) {
    decl->walk(walker);
  }

  return false;
}
