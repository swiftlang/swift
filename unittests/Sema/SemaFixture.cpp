//===--- SemaFixture.cpp - Helper for setting up Sema context --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SemaFixture.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Import.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParseRequests.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/DenseMap.h"

using namespace swift;
using namespace swift::unittest;
using namespace swift::constraints::inference;

SemaTest::SemaTest()
    : Context(*ASTContext::get(
          LangOpts, TypeCheckerOpts, SILOpts, SearchPathOpts, ClangImporterOpts,
          SymbolGraphOpts, CASOpts, SerializationOpts, SourceMgr, Diags)) {
  INITIALIZE_LLVM();

  registerParseRequestFunctions(Context.evaluator);
  registerTypeCheckerRequestFunctions(Context.evaluator);
  registerClangImporterRequestFunctions(Context.evaluator);

  Context.addModuleLoader(ImplicitSerializedModuleLoader::create(Context));
  Context.addModuleLoader(ClangImporter::create(Context), /*isClang=*/true);

  auto *stdlib = Context.getStdlibModule(/*loadIfAbsent=*/true);
  assert(stdlib && "Failed to load standard library");

  DC = ModuleDecl::create(Context.getIdentifier("SemaTests"), Context,
                          [&](ModuleDecl *M, auto addFile) {
    auto bufferID = Context.SourceMgr.addMemBufferCopy("// nothing\n");
    MainFile = new (Context) SourceFile(*M, SourceFileKind::Main, bufferID);

    AttributedImport<ImportedModule> stdlibImport{
        {ImportPath::Access(), stdlib},
        /*options=*/{}};

    MainFile->setImports(stdlibImport);
    addFile(MainFile);
  });
}

Type SemaTest::getStdlibType(StringRef name) const {
  auto typeName = Context.getIdentifier(name);

  auto *stdlib = Context.getStdlibModule();

  llvm::SmallVector<ValueDecl *, 4> results;
  stdlib->lookupValue(typeName, NLKind::UnqualifiedLookup, results);

  if (results.size() != 1)
    return Type();

  if (auto *decl = dyn_cast<TypeDecl>(results.front())) {
    if (auto *NTD = dyn_cast<NominalTypeDecl>(decl))
      return NTD->getDeclaredType();
    return decl->getDeclaredInterfaceType();
  }

  return Type();
}

NominalTypeDecl *SemaTest::getStdlibNominalTypeDecl(StringRef name) const {
  auto typeName = Context.getIdentifier(name);

  auto *stdlib = Context.getStdlibModule();

  llvm::SmallVector<ValueDecl *, 4> results;
  stdlib->lookupValue(typeName, NLKind::UnqualifiedLookup, results);

  if (results.size() != 1)
    return nullptr;

  return dyn_cast<NominalTypeDecl>(results.front());
}

VarDecl *SemaTest::addExtensionVarMember(NominalTypeDecl *decl,
                                         StringRef name, Type type) const {
  auto *ext = ExtensionDecl::create(Context, SourceLoc(), nullptr, { }, DC,
                                    nullptr);
  decl->addExtension(ext);
  ext->setExtendedNominal(decl);

  auto *VD = new (Context) VarDecl(/*isStatic=*/ true, VarDecl::Introducer::Var,
                                   /*nameLoc=*/ SourceLoc(),
                                   Context.getIdentifier(name), ext);

  ext->addMember(VD);
  auto *pat = new (Context) NamedPattern(VD);
  VD->setNamingPattern(pat);
  pat->setType(type);

  return VD;
}

ProtocolType *SemaTest::createProtocol(llvm::StringRef protocolName,
                                       Type parent) {
  auto *PD = new (Context)
      ProtocolDecl(DC, SourceLoc(), SourceLoc(),
                   Context.getIdentifier(protocolName),
                   /*PrimaryAssociatedTypeNames=*/{},
                   /*Inherited=*/{},
                   /*trailingWhere=*/nullptr);
  PD->setImplicit();

  return ProtocolType::get(PD, parent, Context);
}

BindingSet SemaTest::inferBindings(ConstraintSystem &cs,
                                   TypeVariableType *typeVar) {
  for (auto *typeVar : cs.getTypeVariables()) {
    auto &node = cs.getConstraintGraph()[typeVar];
    node.resetBindingSet();

    if (!typeVar->getImpl().hasRepresentativeOrFixed())
      node.initBindingSet();
  }

  for (auto *typeVar : cs.getTypeVariables()) {
    auto &node = cs.getConstraintGraph()[typeVar];
    if (!node.hasBindingSet())
      continue;

    auto &bindings = node.getBindingSet();
    bindings.inferTransitiveProtocolRequirements();
    bindings.finalize(/*transitive=*/true);
  }

  auto &node = cs.getConstraintGraph()[typeVar];
  ASSERT(node.hasBindingSet());
  return node.getBindingSet();
}
