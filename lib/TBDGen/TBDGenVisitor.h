//===--- TBDGenVisitor.h - AST Visitor for TBD generation -----------------===//
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
//  This file defines the visitor that finds all symbols in a swift AST.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_TBDGEN_TBDGENVISITOR_H
#define SWIFT_TBDGEN_TBDGENVISITOR_H

#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/Basic/LLVM.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILWitnessTable.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/Triple.h"

using namespace swift::irgen;
using StringSet = llvm::StringSet<>;

namespace swift {
namespace tbdgen {

class TBDGenVisitor : public ASTVisitor<TBDGenVisitor> {
public:
  StringSet &Symbols;
  const llvm::Triple &Triple;
  const UniversalLinkageInfo &UniversalLinkInfo;
  ModuleDecl *SwiftModule;
  StringRef InstallName;

private:
  bool FileHasEntryPoint = false;
  bool SILSerializeWitnessTables;

  bool InsideAbstractStorageDecl = false;

  void addSymbol(StringRef name) {
    auto isNewValue = Symbols.insert(name).second;
    (void)isNewValue;
    assert(isNewValue && "already inserted");
  }

  void addSymbol(SILDeclRef declRef);

  void addSymbol(LinkEntity entity) {
    auto linkage =
        LinkInfo::get(UniversalLinkInfo, SwiftModule, entity, ForDefinition);

    auto externallyVisible =
        llvm::GlobalValue::isExternalLinkage(linkage.getLinkage()) &&
        linkage.getVisibility() != llvm::GlobalValue::HiddenVisibility;

    if (externallyVisible)
      addSymbol(linkage.getName());
  }

  void addConformances(DeclContext *DC);

public:
  TBDGenVisitor(StringSet &symbols, const llvm::Triple &triple,
                const UniversalLinkageInfo &universalLinkInfo,
                ModuleDecl *swiftModule, bool silSerializeWitnessTables,
                StringRef installName)
      : Symbols(symbols), Triple(triple), UniversalLinkInfo(universalLinkInfo),
        SwiftModule(swiftModule), InstallName(installName),
        SILSerializeWitnessTables(silSerializeWitnessTables) {}

  void setFileHasEntryPoint(bool hasEntryPoint) {
    FileHasEntryPoint = hasEntryPoint;

    if (hasEntryPoint)
      addSymbol("main");
  }

  void visitMembers(Decl *D) {
    SmallVector<Decl *, 4> members;
    auto addMembers = [&](DeclRange range) {
      for (auto member : range)
        members.push_back(member);
    };
    if (auto ED = dyn_cast<ExtensionDecl>(D))
      addMembers(ED->getMembers());
    else if (auto NTD = dyn_cast<NominalTypeDecl>(D))
      addMembers(NTD->getMembers());
    else if (auto ASD = dyn_cast<AbstractStorageDecl>(D))
      ASD->getAllAccessorFunctions(members);

    for (auto member : members) {
      ASTVisitor::visit(member);
    }
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD);

  void visitValueDecl(ValueDecl *VD);

  void visitAbstractFunctionDecl(AbstractFunctionDecl *AFD);

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    // any information here is encoded elsewhere
  }

  void visitNominalTypeDecl(NominalTypeDecl *NTD);

  void visitClassDecl(ClassDecl *CD);

  void visitConstructorDecl(ConstructorDecl *CD);

  void visitExtensionDecl(ExtensionDecl *ED);

  void visitProtocolDecl(ProtocolDecl *PD);

  void visitAbstractStorageDecl(AbstractStorageDecl *ASD);

  void visitVarDecl(VarDecl *VD);

  void visitDecl(Decl *D) { visitMembers(D); }
};
} // end namespace tbdgen
} // end namespace swift

#endif
